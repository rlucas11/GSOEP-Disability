ddply(dis, .(dis), function(z){
    data.frame(obsnum = seq_along(z$dis))
})

