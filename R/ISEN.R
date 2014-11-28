TIEMPOS_PUNTUAL <- function(lat, lon, annio, mes, dia, hora, minuto, segundo)
{
   if(!(missing(lat) | missing(lon) | missing(annio) | missing(mes) | missing(dia) | missing(hora) | missing(minuto) | missing(segundo)))
   {
#     # Variables:::::::::::::::::::::::
#     #---------------------------------
#     double _X = -999
#     double _Y = -999
#     #---------------------------------
#     double _HoraDecimal = -999
#     int _HoraEnteraLON = -999
#     double _HoraSexagesimal = -999
#     #---------------------------------
#     double _MinutoDecimal = -999
#     int _MinutoEnteroLON = -999
#     double _MinutoSexagesimal = -999
#     #---------------------------------
#     double _SegundoDecimal = -999
#     int _SegundoEnteroLON = -999
#     double _SegundoSexagesimal = -999
#     #---------------------------------
#     double _OffSetMinutosDecimales
#     double _DiaFraccional
#     #---------------------------------
#     DateTime _FechaHoraLAT
#     DateTime _TiempoSolarVerdadero
#     DateTime _FechaHoraLMT
#     DateTime _TiempoSolarMedio
#     double _EquationOfTime
#     double _EOT     
#     DateTime _FechaCompleta
#     #---------------------------------
      FechaCompletaStr <- paste(annio,'-', mes, '-', dia, ' ', hora, ':', minuto, ':', segundo, sep = '')
      FechaCompleta <- as.POSIXct(FechaCompletaStr, tz = "UTC")
      X <- lon
      Y <- lat
      HoraDecimal <- X / 15
      HoraEnteraLON <- as.integer(HoraDecimal)
      HoraSexagesimal <- HoraEnteraLON

      MinutoDecimal <- HoraDecimal - HoraEnteraLON;
      MinutoSexagesimal <- MinutoDecimal * 60;
      MinutoEnteroLON <- as.integer(MinutoSexagesimal)

      SegundoDecimal <- MinutoSexagesimal - MinutoEnteroLON;
      SegundoSexagesimal <- SegundoDecimal * 60;
      SegundoEnteroLON <- as.integer(SegundoSexagesimal)

      OffSetMinutosDecimales <- (HoraEnteraLON * 60) + MinutoDecimal; # Verdaderamente, este no se utiliza, por lo que los datos estan en UTC y porque se calcula el TSM (Tiempo Solar Medio).
      TiempoSolarMedio <- FechaCompleta + hours(HoraEnteraLON) + minutes(MinutoEnteroLON) + seconds(SegundoEnteroLON)

      DiaFraccional <- ((2 * pi) / 365) * (yday(FechaCompleta) - 1 - 1.5 + ((hour(FechaCompleta) - 12) / 24))
      EOT <- 229.18 * (0.000075 + (0.001868 * cos(DiaFraccional)) - (0.032077 * sin(DiaFraccional)) - (0.014615 * cos(2 * DiaFraccional)) - (0.040849 * sin(2 * DiaFraccional)));  # Ecuación del tiempo (en minutos  decimales)

      EotMinutes <- as.integer(EOT)
      SegundosEOT <- EOT - EotMinutes
      SegundosEOT <- SegundosEOT * 60
      EotSegundos <- as.integer(SegundosEOT)
      # Tiempo solar verdadero (TSV) // Apparent Solar Time // True Solar Time // Local Apparent Time (Solar/Sundial) LAT;       
      TiempoSolarVerdadero = TiempoSolarMedio + minutes(EotMinutes) + seconds(EotSegundos)
      
      #-------------------------->            
      # Consolidando 
      #-------------------------->            
      list(TSV = TiempoSolarVerdadero, TSM = TiempoSolarMedio, EOT = EOT) 
   }

   
}