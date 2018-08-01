#NBC_25
result = matrix(100, ncol = 12, nrow = 24)
rownames(result)=c("austral", "breast", "chess", "cleve", "corral", "crx", "diabetes", "flare", "german", "glass", "glass2", "heart", "hepatitis", "iris", "letter", "lymphograph", "mofn", "satimage", "segment", "shuttle_small", "soybean_large", "vehicle", "vote", "waveform.21")
colnames(result)=c(1:10, "Average", "SD")

for(exp_num in 1:10){
  acc = c(
  NBC_CV(IEM_austral,5),
  NBC_CV(IEM_breast,5),
  NBC_ST(IEM_chess, 1066),
  NBC_CV(IEM_cleve,5),
  NBC_CV(IEM_corral,5),
  NBC_CV(IEM_crx,5),
  NBC_CV(IEM_diabetes,5),
  NBC_CV(IEM_flare,5),
  NBC_CV(IEM_german,5),
  NBC_CV(IEM_glass,5),
  NBC_CV(IEM_glass2,5),
  NBC_CV(IEM_heart,5),
  NBC_CV(IEM_hepatitis,5),
  NBC_CV(IEM_iris,5),
  NBC_ST(IEM_letter, 5000),
  NBC_CV(IEM_lympograph,5),
  NBC_CV(IEM_mofn,5),
  NBC_ST(IEM_satimage,2000),
  NBC_ST(IEM_segment,770),
  NBC_ST(IEM_shuttle, 1934),
  NBC_CV(IEM_soybean,5),
  NBC_CV(IEM_vehicle,5),
  NBC_CV(IEM_vote,5),
  NBC_ST(IEM_wave,4700)
  )
  result[,exp_num] = acc
}
result[,11] = apply(result[ ,1:10], 1, mean)
result[,12] = apply(result[ ,1:10], 1, sd)
