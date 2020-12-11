generate_team = function(data){
  data$Ari <- 0;data$Atl <- 0;data$Bos <- 0;data$Bal <- 0;data$ChC <- 0;
  data$Cin <- 0;data$Cle <- 0;data$Col <- 0;data$CWS <- 0;data$Det <- 0;
  data$Flo <- 0;data$Hou <- 0;data$Kan <- 0;data$LAA <- 0;data$LAD <- 0;
  data$Mil <- 0;data$Min <- 0;data$NYM <- 0;data$NYY <- 0;data$Oak <- 0;
  data$Phi <- 0;data$Pit <- 0;data$SDP <- 0;data$Sea <- 0;data$SFG <- 0;
  data$StL <- 0;data$Tam <- 0;data$Tex <- 0;data$Tor <- 0;data$Was <- 0;
  
  data[home=='Ari',]$Ari <- 1;data[home=='Atl',]$Atl <- 1;data[home=='Bos',]$Bos <- 1
  data[home=='Bal',]$Bal <- 1;data[home=='ChC',]$ChC <- 1;data[home=='Cin',]$Cin <- 1;
  data[home=='Cle',]$Cle <- 1;data[home=='Col',]$Col <- 1;data[home=='CWS',]$CWS <- 1;
  data[home=='Det',]$Det <- 1;data[home=='Flo',]$Flo <- 1;data[home=='Hou',]$Hou <- 1;
  data[home=='Kan',]$Kan <- 1;data[home=='LAA',]$LAA <- 1;data[home=='LAD',]$LAD <- 1;
  data[home=='Mil',]$Mil <- 1;data[home=='Min',]$Min <- 1;data[home=='NYM',]$NYM <- 1;
  data[home=='NYY',]$NYY <- 1;data[home=='Oak',]$Oak <- 1;data[home=='Phi',]$Phi <- 1;
  data[home=='Pit',]$Pit <- 1;data[home=='SDP',]$SDP <- 1;data[home=='Sea',]$Sea <- 1;
  data[home=='SFG',]$SFG <- 1;data[home=='Stl',]$Stl <- 1;data[home=='Tam',]$Tam <- 1;
  data[home=='Tex',]$Tex <- 1;data[home=='Tor',]$Tor <- 1;data[home=='Was',]$Was <- 1
  
  return(data)
}