// This file is part of fdaPDE, a C++ library for physics-informed
// spatial and functional data analysis.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <fdaPDE/core.h>   
#include <cstddef>
#include <iomanip>
#include <chrono>
using fdapde::core::advection;
using fdapde::core::diffusion;
using fdapde::core::dt;
using fdapde::core::FEM;
using fdapde::core::fem_order;
using fdapde::core::SPLINE;
using fdapde::core::bilaplacian;
using fdapde::core::laplacian;
using fdapde::core::PDE;
using fdapde::core::Triangulation;
using fdapde::core::spline_order;
using fdapde::core::DiscretizedMatrixField;
using fdapde::core::DiscretizedVectorField;
using fdapde::core::Grid;

#include "../../fdaPDE/models/sampling_design.h"
#include "../../fdaPDE/models/regression/strpde.h"
#include "../../fdaPDE/models/regression/gcv.h"
using fdapde::models::STRPDE;
using fdapde::models::SpaceOnly;
using fdapde::models::ExactEDF;
using fdapde::models::StochasticEDF;
using fdapde::models::Sampling;
using fdapde::models::SpaceTime;
using fdapde::models::SpaceTimeSeparable;
using fdapde::models::not_nan;
#include "../../fdaPDE/calibration/gcv.h"
#include "utils/constants.h"
#include "utils/mesh_loader.h"
#include "utils/utils.h"
using fdapde::testing::almost_equal;
using fdapde::testing::MeshLoader;
using fdapde::testing::read_mtx;
using fdapde::testing::read_csv;

int main() { 
    // define temporal and spatial domain
    double t0 = 1;
    double tf = 32;
    unsigned int M = 13; 
    Triangulation<1, 1> time_mesh(t0, tf, M-1);
    MeshLoader<Triangulation<2, 2>> domain("mesh_geospatial");

    std::string data_path = "../data/models/strpde/geospatial";
    std::string solution_path = "../data/models/strpde/geospatial/results";

    // import data from files
    DMatrix<double> y = read_csv<double>(data_path + "/y.csv");
    DMatrix<double> time_locs = read_csv<double>(data_path + "/time_locs.csv"); 
    DMatrix<double> space_locs = read_csv<double>(data_path + "/space_locs.csv");

    std::cout << "Dim y " << y.rows() << " , " << y.cols() << std::endl ;  
    std::cout << "Dim time_locs " << time_locs.rows() << " , " << time_locs.cols() << std::endl ;    
    
    // define lambda sequences
    std::vector<DVector<double>> lambdas_d_t; std::vector<double> lambdas_d; std::vector<double> lambdas_t;
    
    for(double xs = -6.0; xs <= 0.1; xs += 0.2)
        lambdas_d.push_back(std::pow(10,xs));

    for(double xt = -3.0; xt <= +1.0; xt += 2.0)
        lambdas_t.push_back(std::pow(10,xt));   

    for(auto i = 0; i < lambdas_d.size(); ++i)
        for(auto j = 0; j < lambdas_t.size(); ++j) 
            lambdas_d_t.push_back(SVector<2>(lambdas_d[i], lambdas_t[j]));

    // define matrix of lambdas
    std::cout << "define lambda matrix, with lambdaD.size = " << lambdas_d.size() << ", lambdas_t.size() = " << lambdas_t.size() << std::endl ; 
    DMatrix<double> lambdas(lambdas_d.size()*lambdas_t.size(), 2);
    for (int i = 0; i < lambdas_d.size(); ++i) {
        for (int j = 0; j < lambdas_t.size(); ++j) { 
            lambdas(i * lambdas_t.size() + j, 0) = lambdas_d[i];
            lambdas(i * lambdas_t.size() + j, 1) = lambdas_t[j];
        }
    }


    // define pde
    auto Ld = -laplacian<FEM>();
    DMatrix<double> u = DMatrix<double>::Zero(domain.mesh.n_cells() * 3, 1);
    PDE<Triangulation<2, 2>, decltype(Ld), DMatrix<double>, FEM, fem_order<1>> space_penalty(domain.mesh, Ld, u);


    // define regularizing PDE in time
    auto Lt = -bilaplacian<SPLINE>();
    PDE<Triangulation<1, 1>, decltype(Lt), DMatrix<double>, SPLINE, spline_order<3>> time_penalty(time_mesh, Lt);

    // define model
    STRPDE<SpaceTimeSeparable, fdapde::monolithic> model(space_penalty, time_penalty, Sampling::pointwise);

    model.set_temporal_locations(time_locs);
    model.set_spatial_locations(space_locs);


    // set model's data
    BlockFrame<double, int> df;
    df.stack(OBSERVATIONS_BLK, y);
    model.set_data(df);


    // init smoothing problem
    std::cout << "Model init " << std::endl ; 
    model.init();
    

    // stochastic
    std::cout << "define gcv model " << std::endl ; 
    std::size_t seed = 438172;
    unsigned int MC_run = 500; 
    auto GCV = model.gcv<StochasticEDF>(MC_run, seed);

    // optimize GCV
    Grid<fdapde::Dynamic> opt;

    
    // Optimize
    std::cout << "Optimize " << std::endl ; 
    opt.optimize(GCV, lambdas);
    SVector<2> best_lambda = opt.optimum();

    // Save lambda sequence 
    std::ofstream fileLambda_S_Seq(solution_path + "/lambdas_S_seq.csv");
    for(std::size_t i = 0; i < lambdas_d.size(); ++i) 
        fileLambda_S_Seq << std::setprecision(16) << lambdas_d[i] << "\n"; 
    fileLambda_S_Seq.close();

    std::ofstream fileLambda_T_Seq(solution_path + "/lambdas_T_seq.csv");
    for(std::size_t i = 0; i < lambdas_t.size(); ++i) 
        fileLambda_T_Seq << std::setprecision(16) << lambdas_t[i] << "\n"; 
    fileLambda_T_Seq.close();

    // Save Lambda opt
    std::ofstream fileLambdaoptS(solution_path + "/lambdaS_opt.csv");
    if(fileLambdaoptS.is_open()){
        fileLambdaoptS << std::setprecision(16) << best_lambda[0];
        fileLambdaoptS.close();
    }
    std::ofstream fileLambdaoptT(solution_path + "/lambdaT_opt.csv");
    if (fileLambdaoptT.is_open()){
        fileLambdaoptT << std::setprecision(16) << best_lambda[1];
        fileLambdaoptT.close();
    }
    // Save GCV scores
    std::ofstream fileGCV_scores(solution_path + "/gcv_scores.csv");
    for(std::size_t i = 0; i < GCV.gcvs().size(); ++i) 
        fileGCV_scores << std::setprecision(16) << std::sqrt(GCV.gcvs()[i]) << "\n" ; 

    fileGCV_scores.close();



    // Run 
    model.set_lambda_D(best_lambda[0]);
    model.set_lambda_T(best_lambda[1]);

    model.init();
    model.solve();


    // Save solution
    std::cout << "Save solution" << std::endl;
    DMatrix<double> computedF = model.f();
    const static Eigen::IOFormat CSVFormatf(Eigen::FullPrecision, Eigen::DontAlignCols, ", ", "\n");
    std::ofstream filef(solution_path + "/f.csv");
    if(filef.is_open()){
        filef << computedF.format(CSVFormatf);
        filef.close();
    }

    DMatrix<double> computedFn = model.Psi()*model.f();
    const static Eigen::IOFormat CSVFormatfn(Eigen::FullPrecision, Eigen::DontAlignCols, ", ", "\n");
    std::ofstream filefn(solution_path + "/fn.csv");
    if(filefn.is_open()){
        filefn << computedFn.format(CSVFormatfn);
        filefn.close();
    }

    return 0;    

}
