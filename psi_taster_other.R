# load additional Tastermonial data files

nutrisense_sherri <- glucose_df_from_nutrisense(filepath = file.path(config::get("tastermonial")$datadir,
                                                       "1627653419114_export-SherriJo.csv"))

nutrisense_anthony <- glucose_df_from_nutrisense(filepath = file.path(config::get("tastermonial")$datadir,
                                                       "nutrisense_anthony_davis.csv"),
                                  tz = Sys.timezone())
