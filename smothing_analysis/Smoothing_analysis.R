## Analyse results from smoothing

rm(list=ls())

library(sf)
library(ggplot2)
library(plot3D)
library(fdaPDE)
library(viridis)

plot_segments = function(geonodes = less_geo_nodes, segments, last = 0,  
                         xlim = NULL, ylim = NULL, add = FALSE, 
                         add_to_previous = FALSE, add_locations = NULL, 
                         col = "black", lwd = 2, asp=1.4, xlab = "", ylab = ""){
  if(!add){
    if(is.null(xlim)){
      plot(geonodes[,1], geonodes[,2], col="grey", pch = 19, 
           cex = 0.8, xlab = xlab, ylab = ylab, xaxt = 'n', yaxt = 'n', asp=asp)
      if(!is.null(add_locations)){
        points(add_locations[,1], add_locations[,2], col="purple", pch = 19, asp=asp)
      }
    }
    if(!is.null(xlim) && !is.null(ylim)){
      plot(geonodes[,1], geonodes[,2], col="grey", pch = 19, cex = 0.8, 
           xlim = xlim, ylim = ylim, xlab=xlab, ylab=ylab, xaxt = 'n', yaxt = 'n', asp=asp)
      if(!is.null(add_locations)){
        points(add_locations[,1], add_locations[,2], col="purple", pch = 19, asp=asp)
      }
    }
    
    
  }
  first = TRUE
  for(i in 1:(dim(segments)[1]-last)){
    x0 = geonodes[segments[i,1],1]
    y0 = geonodes[segments[i,1],2]
    x1 = geonodes[segments[i,2],1]
    y1 = geonodes[segments[i,2],2]
    if(!add_to_previous){
      if(first){
        segments2D(x0 = x0, y0 = y0, x1 = x1, y1 = y1, col=col, lwd = lwd, 
                   xlim = c(min(geonodes[,1]), max(geonodes[,1])), 
                   ylim = c(min(geonodes[,2]), max(geonodes[,2])), 
                   xaxt = 'n', yaxt = 'n', asp=asp, xlab=xlab, ylab=ylab)
        first = FALSE
      } else{
        segments2D(x0 = x0, y0 = y0, x1 = x1, y1 = y1, col=col, lwd = lwd, add = TRUE, asp=asp, xlab=xlab, ylab=ylab)
      }
    }else{
      segments2D(x0 = x0, y0 = y0, x1 = x1, y1 = y1, col=col, lwd = lwd, add = TRUE)
    }
    
    
  }
}

image.spaceonly_Cpp=function(mesh, f_hat,
                             FEMbasis, FEMbasis_lombardy,
                             Nx = 100, Ny = 100, xlim = NA, 
                             col_levels = "black", col = rev(magma(256)),
                             drawlables.numbers = TRUE, 
                             add_boundary = FALSE, 
                             lombardy_nodes = NULL, lombardy_segments = NULL,
                             ylim = NA, zlim = NULL, out=NULL, lwd.contour.lines = 1, 
                             nlev = 10, levels = NULL, draw_levels=TRUE, ...){                                  
  if(is.na(xlim[1]))
  {
    xmin = min(mesh$nodes[,1])-0.1
    xmax = max(mesh$nodes[,1])+0.1
  }
  else
  {
    xmin = xlim[1]-0.1
    xmax = xlim[2]+0.1
  }
  
  if(is.na(ylim[1]))
  {
    ymin = min(mesh$nodes[,2])-0.1
    ymax = max(mesh$nodes[,2])+0.1
  }
  else
  {
    ymin = ylim[1]-0.1
    ymax = ylim[2]+0.1
  }
  
  X    = matrix(seq(xmin, xmax, len=Nx),ncol=1)
  Y    = matrix(seq(ymin, ymax, len=Ny),ncol=1)    
  
  Xmat = X %*% matrix(1,nrow=1,ncol=Ny)
  Ymat = matrix(1,nrow=Nx,ncol=1) %*% t(Y)
  Xvec = NULL
  for (numc in 1:Ny)
  {
    Xvec=c(Xvec,Xmat[,numc])
  }
  Yvec = NULL
  for (numc in 1:Ny)
  {
    Yvec=c(Yvec,Ymat[,numc])
  }
  
  eval_points = cbind(Xvec, Yvec)
  eval_sol=rep(NA,nrow(eval_points))
  if(!is.null(out))
    eval_points = eval_points[-out,]
  
  # Build and evaluate FEM object from f_hat 
  eval_sol_in = eval.FEM(FEM(f_hat, FEMbasis), locations = eval_points) 
  
  if(!is.null(out))
    eval_sol[(1:length(eval_sol))[-out]]=eval_sol_in
  else{
    eval_sol=eval_sol_in
  }
  
  # Mask 
  f_hat_lombardy = rep(0, dim(FEMbasis_lombardy$mesh$nodes)[1])
  eval_mask_in = eval.FEM(FEM(f_hat_lombardy, FEMbasis_lombardy), locations = eval_points) 
  
  outside = unique(c(which(is.na(eval_sol)), which(is.na(eval_mask_in))))
  if(length(outside) > 0)
    eval_sol[(1:length(eval_sol))[outside]]= NA
  
  evalmat = matrix(eval_sol, nrow=Nx, ncol=Ny, byrow=F)
  
  
  if(is.null(zlim))
  {
    zlim[1] = min(eval_sol, na.rm = TRUE)
    zlim[2] = max(eval_sol, na.rm = TRUE)
  }
  
  cat("zlim in image.spaceonly_Cpp = ", zlim[1], ";", zlim[2], "\n")
  
  image2D(z=evalmat,x=as.vector(X),y=as.vector(Y),zlim=zlim, col=col, ...)
  if(draw_levels){
    contour2D(z=evalmat,x=as.vector(X),y=as.vector(Y),
              zlim=zlim, add=T,colkey=F,col=col_levels,nlevels=nlev,
              levels=levels, labcex=cex_contour, lwd = lwd.contour.lines)
  }
  
  
  if(add_boundary){
    plot_segments(lombardy_nodes, lombardy_segments, xlim = c(xmin, xmax), ylim = c(ymin, ymax),
                  lwd = 2, asp = 1.0, col = "black", add = FALSE, add_to_previous = TRUE)
  }
}

# Mesh smoothing
mesh = readRDS(paste0(getwd(), "/mesh_geospatial/mesh.RData"))
FEMbasis_fit <-create.FEM.basis(mesh)

nodes = mesh$nodes
new_nodes_canotto = cbind(nodes[,1]*(7/5), nodes[,2])
nnodes= dim(new_nodes_canotto)[1]
range(new_nodes_canotto[,1])
FEMbasis_fit$mesh$nodes = new_nodes_canotto

# Import lombardy segments 
mesh_lombardy = readRDS(paste0(getwd(), "/mesh_lombardy/mesh.RData"))
plot(mesh_lombardy, asp=1)
lombardy_nodes = mesh_lombardy$nodes   # 436
lombardy_nodes[,1] = lombardy_nodes[,1]*(7/5)
new_lombardy_nodes = lombardy_nodes
lombardy_segments = mesh_lombardy$segments
FEMbasis_lombardy = create.FEM.basis(mesh_lombardy)
FEMbasis_lombardy$mesh$nodes = new_lombardy_nodes

# Refine mesh
mesh_lombardy_fine = refine.mesh.2D(mesh_lombardy, minimum_angle = 20, maximum_area = 0.00025) 
mesh_lombardy_fine$nodes[,1] = mesh_lombardy_fine$nodes[,1]*(7/5)
nodes_plot_fine = mesh_lombardy_fine$nodes  # 
range(nodes_plot_fine)  

# Import locations
locs= read.csv(paste0(getwd(), "/smoothing/space_locs.csv"), header=TRUE)[, -1]
n = dim(locs)[1]

# time domain
tf=32
m = tf
M = 13
time_mesh = seq(1, tf, length.out=M)


# import data
dati = as.matrix(read.csv(paste0(getwd(), "/smoothing/y.csv"), header=TRUE))[, -1]
dati = matrix(as.numeric(dati), nrow=n, ncol=m)
range(dati, na.rm=TRUE)

time_locs = seq(1, ncol(dati))
m = length(time_locs)


# Read GCV 
sol_dir = paste0(getwd(), "/smoothing/results")

# read gcv results
gcv_scores = read.csv(paste0(sol_dir, "/gcv_scores.csv"), header = FALSE)$V1
seq_lambdaS = read.csv(paste0(sol_dir, "/lambdas_S_seq.csv"), header = FALSE)$V1
seq_lambdaT = read.csv(paste0(sol_dir, "/lambdas_T_seq.csv"), header = FALSE)$V1
suppressWarnings({
  lambdaS_opt = read.csv(paste0(sol_dir, "/lambdaS_opt.csv"), header = FALSE)$V1
  lambdaT_opt = read.csv(paste0(sol_dir, "/lambdaT_opt.csv"), header = FALSE)$V1
})


cat("\n Lambda opt = (", lambdaS_opt, " , ", lambdaT_opt, ")")


# Plot gcv scores 
z = matrix(gcv_scores, 
           nrow = length(seq_lambdaS), ncol = length(seq_lambdaT), byrow = TRUE)

par(mfrow = c(1, ncol(z)))
for(j in 1:ncol(z)){
  plot(x = log10(seq_lambdaS), y = z[, j], pch=19,
       type="b", ylab="", ylim=c(10, 30))
  abline(v = log10(seq_lambdaS[which.min(z[,j])]), lty=2, col="red")
}
# -> good selection of smoothing parameters


# Evaluate the smoothing result over a fine grid to proceed with the analysis
f = read.csv(paste0(sol_dir, "/f.csv"), header=FALSE)$V1
time_eval = time_locs
f_eval_fine = eval.FEM.time(FEM.time(f, time_mesh, FEMbasis_fit),
                            space.time.locations = cbind(rep(time_eval, each = nrow(nodes_plot_fine)),
                                                         rep(nodes_plot_fine[,1], length(time_eval)),
                                                         rep(nodes_plot_fine[,2], length(time_eval))))
range(f_eval_fine)

f_eval_mat_fine = matrix(f_eval_fine, nrow= dim(nodes_plot_fine)[1], ncol=m)
write.csv(format(f_eval_mat_fine, digits=16), file=paste0(getwd(), "/results/f_eval_fine2.csv"))
write.csv(format(nodes_plot_fine, digits=16), file=paste0(getwd(), "/results/nodes_eval_fine2.csv"))