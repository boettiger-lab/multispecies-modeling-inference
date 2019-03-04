# code for simulating co-occurrence of two interacting species following Araujo & Rozenfeld (2014)
sim.Dist = function(L=100,rhoA=0.3,rhoB=0.3,Ia=0,Ib=0,output='all',agg=NULL) {
  # currently not considered: spatial autocorrelation, exclusive ranges
  # output = 'all' include plotting of landscape
  # output = 'cooccAB' only outputs the co-occurrence index
  # output = 'cooccAB.null" outputs co-occurrence index and a T/F value indicating
  # correspondence of rhoAB with null expectation 
  land = matrix(1,nrow=L,ncol=L)
  #---------------------------------------------------------------------------------------
  n.A.cur = 0 # current number of ind A in landscape
  n.A.target = floor(rhoA * L^2) # expected number of ind A in landscape given prevalence rhoA
  n.A.ToTarget = n.A.target - n.A.cur
  # randomly distribute species A
  land[sample(seq_len(prod(dim(land)))[land==1],n.A.ToTarget)] = 2
  #---------------------------------------------------------------------------------------
  n.B.cur = 0
  n.B.target = floor(rhoB * L^2)
  #---------------------------------------------------------------------------------------
  # calculate amount of co-occurrence cells according to eqs. 3-5
  rhoAB.null <- rhoA * rhoB
  if (Ia > 0) {
    if (Ib > 0) {
      # (+/+)
      rhoAB <- rhoA * rhoB + max(Ia, Ib) * (min(rhoA, rhoB) - rhoA * rhoB)
    } else {
      # (+/-)
      rhoAB <- (rhoA * rhoB + Ia * (min(rhoA, rhoB) - rhoA * rhoB)) * (1 + Ib)
    }
  } else {
    5
    if (Ib > 0) {
      # (-/+)
      rhoAB <- (rhoA * rhoB + Ib * (min(rhoA, rhoB) - rhoA * rhoB)) * (1 + Ia)
    } else {
      # (-/-)
      rhoAB <- rhoA * rhoB * (1 + min(Ia, Ib))
    }
  }
  n.AB.target <- rhoAB * L^2
  
  #---------------------------------------------------------------------------------------
  # randomly sample co-occurrence cells and cells only occupied by species B
  land[sample(seq_len(prod(dim(land)))[land == 2], n.AB.target)] <- 4
  n.B.cur <- sum(land == 4)
  n.B.ToTarget <- n.B.target - n.B.cur
  # randomly distribute species B
  land[sample(seq_len(prod(dim(land)))[land == 1], n.B.ToTarget)] <- 3
  cooccAB <- sum(land == 4) / (sum(land == 3) + sum(land == 2) + sum(land == 4))
  #---------------------------------------------------------------------------------------
  # spatial dependence?
  if (!is.null(agg)) {
    require(raster)
    agg.land <- function(agg.y) {
      land.r <- raster(land, xmn = 0, xmx = L, ymn = 0, ymx = L)
      aggregate(land.r, agg.y, function(x, ...) {
        ifelse(4 %in% x, 4, ifelse(2 %in% x & 3 %in% x, 4, max(x)))
      })
    }
    agg.land.true <- function(agg.y) {
      land.r <- raster(land, xmn = 0, xmx = L, ymn = 0, ymx = L)
      aggregate(land.r, agg.y, function(x, ...) {
        ifelse(4 %in% x, 4, max(x))
      })
    }
    agg.l <- lapply(agg, function(agg.y) {
      agg.land(agg.y)
    })
    names(agg.l) <- paste0("agg", agg)
    agg.l.true <- lapply(agg, function(agg.y) {
      agg.land.true(agg.y)
    })
    names(agg.l.true) <- paste0("agg", agg)
  
    #---------------------------------------------------------------------------------------
    ---------
      # output
  
      if (output == "all") {
        require(fields)
        #
        print(image.plot(land,col=c('white','grey60','grey30','red'),breaks=c(.5,1.5,2.5,3.5,4.5
        ),axes=F,axis.args=list(at=1:4,labels=c('Empty','A','B','A & B'))))
        if (is.null(agg)) {
          return(list(
            rhoAB = rhoAB, rhoAB.null = rhoAB.null, coocc =
              cooccAB, land = land
          ))
        } else {
          return(list(
            rhoAB = rhoAB,
            rhoAB.null = rhoAB.null,
            coocc = cooccAB, land = land,
            land.agg = lapply(agg.l, function(x) {
              data.frame(coordinates(x), cells = values(x))
            }),
            land.agg.true = lapply(agg.l.true, function(x) {
              data.frame(coordinates(x), cells = values(x))
            })
          ))
        }
      }
      else if (output == "cooccAB") {
        return(cooccAB)
      } else if (output == "cooccAB.null") return(list(coocc = cooccAB, null = rhoAB / rhoAB.null))
  }
}
examp <- sim.Dist(rhoA = 0.3, rhoB = 0.3, Ia = .5, Ib = .5, output = "all", agg = c(2, 4))