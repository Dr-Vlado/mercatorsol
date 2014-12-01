ISE_PUNTUAL <- function(lat, lon, annio, mes, dia, hora, minuto, segundo)
{
   if(!(missing(lat) | missing(lon) | missing(annio) | missing(mes) | missing(dia) | missing(hora) | missing(minuto) | missing(segundo)))
   {
      ConstanteSOLAR <- 1367
      DegToRad <- 57.295779513
      RadToDeg <- 0.0174532925
      PresionEnSuperficie_Dada <- 1013.0 # para la refracción: al final no se utiliza.
      Temperatura_Dada <- 15.0 # idem
      Aspect <- 180.0
      X <- lon
      Y <- lat
      FechaCompletaStr <- paste(annio,'-', mes, '-', dia, ' ', hora, ':', minuto, ':', segundo, sep = '')
      FechaCompleta <- as.POSIXct(FechaCompletaStr, tz = "UTC")
      
      DayAng <- 360.0 * (yday(FechaCompleta) - 1) / 365.0
      Double_DayAng <- 2.0 * DayAng
      Sin_DayAng <- sin(RadToDeg * DayAng)
      Cos_DayAng <- cos(RadToDeg * DayAng)
      Cos_Double_DayAng <- cos(RadToDeg * Double_DayAng)
      Sin_Double_DayAng <- sin(RadToDeg * Double_DayAng)
      EarthRadiusVector <- 1.000110 + 0.034221 * Cos_DayAng + 0.001280 * Sin_DayAng
      EarthRadiusVector <- EarthRadiusVector + 0.000719 * Cos_Double_DayAng + 0.000077 * Sin_Double_DayAng
      
      UTime_UniversalCoordinatedTime <- (hour(FechaCompleta) * 3600.0) + (minute(FechaCompleta) * 60.0) + second(FechaCompleta)
      UTime_UniversalCoordinatedTime <- UTime_UniversalCoordinatedTime / 3600
      Delta_JD <- year(FechaCompleta) - 1949
      Salto_JD = as.integer(Delta_JD / 4.0)
      DiaJuliano_JD = 32916.5 + Delta_JD * 365.0 + Salto_JD + yday(FechaCompleta) + UTime_UniversalCoordinatedTime / 24.0
      TiempoParaCoordenadasEclipticas_EC <- DiaJuliano_JD - 51545.0
      LongitudMedia_EC <- 280.460 + 0.9856474 * TiempoParaCoordenadasEclipticas_EC      
      LongitudMedia_EC <- LongitudMedia_EC - 360.0 * as.integer(LongitudMedia_EC / 360.0)   
      if (LongitudMedia_EC < 0.0)      
         LongitudMedia_EC <- LongitudMedia_EC + 360.0
      
      AnomaliaMedia_EC <- 357.528 + 0.9856003 * TiempoParaCoordenadasEclipticas_EC
      AnomaliaMedia_EC <- AnomaliaMedia_EC - 360.0 * as.integer(AnomaliaMedia_EC / 360.0)
      if (AnomaliaMedia_EC < 0.0)
            AnomaliaMedia_EC <- AnomaliaMedia_EC + 360.0
      
      LongitudECliptica <- LongitudMedia_EC + 1.915 * sin(AnomaliaMedia_EC * RadToDeg) + 0.020 * sin(2.0 * AnomaliaMedia_EC * RadToDeg)
      LongitudECliptica <- LongitudECliptica - 360.0 * as.integer(LongitudECliptica / 360.0)
      if (LongitudECliptica < 0.0)
         LongitudECliptica <- LongitudECliptica + 360.0
      
      OblicuidadDeLaEcliptica <- 23.439 - 0.0000004 * TiempoParaCoordenadasEclipticas_EC
      Declinacion <- DegToRad * asin(sin(OblicuidadDeLaEcliptica * RadToDeg) *sin(LongitudECliptica * RadToDeg))
      Tope <- cos(RadToDeg * OblicuidadDeLaEcliptica) * sin(RadToDeg * LongitudECliptica)
      Fondo <- cos(RadToDeg * LongitudECliptica)
      AscensionRecta <- DegToRad * atan2(Tope, Fondo)
      if (AscensionRecta < 0.0)
         AscensionRecta <- AscensionRecta + 360
      
      TiempoSideralGreenwich <- 6.697375 + 0.0657098242 * TiempoParaCoordenadasEclipticas_EC + UTime_UniversalCoordinatedTime
      TiempoSideralGreenwich <- TiempoSideralGreenwich - 24.0 * as.integer(TiempoSideralGreenwich / 24.0)
      if (TiempoSideralGreenwich < 0.0) # VER REF (01-DIC-2014)...
            TiempoSideralGreenwich <- TiempoSideralGreenwich + 24
      
      TiempoSideralLocalMedio <- TiempoSideralGreenwich * 15.0 + X
      TiempoSideralLocalMedio <- TiempoSideralLocalMedio - 360.0 * as.integer(TiempoSideralLocalMedio / 360.0)
      if ( TiempoSideralLocalMedio < 0.0)
         TiempoSideralLocalMedio <- TiempoSideralLocalMedio + 360.0
      
      AnguloDeHora <- TiempoSideralLocalMedio - AscensionRecta
      
      if (AnguloDeHora < -180.0)      
         AnguloDeHora <- AnguloDeHora + 360.0
      
      if (AnguloDeHora > 180.0)
            AnguloDeHora <- AnguloDeHora - 360.0
                  
      Cos_AnguloZeithSolar <- sin(RadToDeg * Declinacion) * sin(RadToDeg * Y) + cos(RadToDeg * Declinacion) * cos(RadToDeg * Y) * cos(RadToDeg * AnguloDeHora)
      if (abs(Cos_AnguloZeithSolar) > 1.0)
      {
         if (Cos_AnguloZeithSolar >= 0.0)            
               Cos_AnguloZeithSolar = 1.0         
         else
            Cos_AnguloZeithSolar = -1.0         
      }
      
      AnguloZenithSolarExtraterrestre <- acos(Cos_AnguloZeithSolar) * DegToRad
      if (AnguloZenithSolarExtraterrestre > 99.0)
         AnguloZenithSolarExtraterrestre <- 99.0
      
      ElevacionExtraterrestre <- 90.0 - AnguloZenithSolarExtraterrestre  
      if (ElevacionExtraterrestre > 85)
      {
         CorreccionRefraccion <- 0.0
      }
      else
      {
         Tan_ElevacionExtraterrestre <- tan(RadToDeg * ElevacionExtraterrestre)
         if (ElevacionExtraterrestre >= 5)
         {
            CorreccionRefraccion <- 58.1 / Tan_ElevacionExtraterrestre - 0.07 / (Tan_ElevacionExtraterrestre^3) + 0.000086 / (Tan_ElevacionExtraterrestre^5)
         }
         else if (ElevacionExtraterrestre >= -0.575)
         {
            CorreccionRefraccion <- 1735.0 + ElevacionExtraterrestre * ( -518.2 + ElevacionExtraterrestre * (103.4 + ElevacionExtraterrestre * ( - 12.79 + ElevacionExtraterrestre * 0.711)))
         }
         else
         {
            CorreccionRefraccion <- -20.774 / Tan_ElevacionExtraterrestre
         }
      }   
         
      PresTemp <- (PresionEnSuperficie_Dada * 283.0) / (1013.0 * (273.0 + Temperatura_Dada))
      CorreccionRefraccion <- CorreccionRefraccion * PresTemp / 3600
      
      AnguloElevacionSolarREFRACTADO <- ElevacionExtraterrestre + CorreccionRefraccion
      
      if (AnguloElevacionSolarREFRACTADO < -9.0)
      {
         AnguloElevacionSolarREFRACTADO <- -9.0
      }
      
      AnguloZenithREFRACTADO <- 90 - AnguloElevacionSolarREFRACTADO
      Cos_AnguloZenithREFRACTADO <- cos(RadToDeg * AnguloZenithREFRACTADO)
      
      if (Cos_AnguloZenithREFRACTADO > 0.0)
      {
         IrradianciaSolarExtraterrestre_N <- ConstanteSOLAR * EarthRadiusVector
         IrradianciaSolarExtraterrestreRAFRACTADA <- IrradianciaSolarExtraterrestre_N * Cos_AnguloZenithREFRACTADO
         IrradianciaSolarExtraterrestre <- IrradianciaSolarExtraterrestre_N * Cos_AnguloZeithSolar
      }
      else
      {
         IrradianciaSolarExtraterrestre_N <- 0.0
         IrradianciaSolarExtraterrestreRAFRACTADA <- 0.0
         if (Cos_AnguloZeithSolar <= 0.0)
         {
            IrradianciaSolarExtraterrestre <- 0.0
         }
      }   
      list(ISEH = IrradianciaSolarExtraterrestre, ISEH_R = IrradianciaSolarExtraterrestreRAFRACTADA, ISEN = IrradianciaSolarExtraterrestre_N) 
   }   
}

TIEMPOS_PUNTUAL <- function(lat, lon, annio, mes, dia, hora, minuto, segundo)
{
   if(!(missing(lat) | missing(lon) | missing(annio) | missing(mes) | missing(dia) | missing(hora) | missing(minuto) | missing(segundo)))
   {
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
      TiempoSolarMedio <- FechaCompleta + hours(HoraEnteraLON) + minute(MinutoEnteroLON) + second(SegundoEnteroLON)
      
      DiaFraccional <- ((2 * pi) / 365) * (yday(FechaCompleta) - 1 - 1.5 + ((hour(FechaCompleta) - 12) / 24))
      EOT <- 229.18 * (0.000075 + (0.001868 * cos(DiaFraccional)) - (0.032077 * sin(DiaFraccional)) - (0.014615 * cos(2 * DiaFraccional)) - (0.040849 * sin(2 * DiaFraccional)));  # Ecuación del tiempo (en minutos  decimales)
      
      EotMinutes <- as.integer(EOT)
      SegundosEOT <- EOT - EotMinutes
      SegundosEOT <- SegundosEOT * 60
      EotSegundos <- as.integer(SegundosEOT)
      # Tiempo solar verdadero (TSV) // Apparent Solar Time // True Solar Time // Local Apparent Time (Solar/Sundial) LAT;       
      TiempoSolarVerdadero = TiempoSolarMedio + minute(EotMinutes) + second(EotSegundos)
      
      #-------------------------->            
      # Consolidando 
      #-------------------------->            
      list(TSV = TiempoSolarVerdadero, TSM = TiempoSolarMedio, EOT = EOT) 
   }   
}