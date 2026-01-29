call_detections(
  results = buzzr::read_results('models/model_general_v3/output/testbuzz_buzzdetect.csv'),
  thresholds = c(ins_buzz = -1.2, ambient_rain = -1)
)
