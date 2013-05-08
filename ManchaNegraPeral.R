# "If you torture the data long enough it will eventually confess."
# -Ronald Harry Coase -

library(sendmailR)
sendmail_options(smtpServerSMTP ="correo6.larioja.org", smtpPortSMTP = "25")
from <- "<labsiar@rioja.org>"

# funcion de cálculo del índice de la mancha negra

calcularIndiceBSPCast <- function(temperatura, humectacion){
    # temperatura: vector de valores horarios, de 9:00 a 8:00 HS (segun modelo)
    # humectacion: vector de valores horarios, de 9:00 a 8:00 HS (segun modelo)
	
        totalHumectacion <- sum(humectacion)

        tempHumec <- cbind(temperatura, humectacion)
        TAirMdHumect <- mean(tempHumec[tempHumec[,2] > 0,1])

        logS <- -1.70962+ 0.0289*TAirMdHumect + 0.04943*totalHumectacion +
		0.00868*totalHumectacion*TAirMdHumect - 0.002362*totalHumectacion^2 +
		- 0.000238*TAirMdHumect^2*totalHumectacion
        indRiesgo <- (10^(logS))/3.7942

	if(is.na(indRiesgo)) indRiesgo <- 0

        return(c(TAirMdHumect, totalHumectacion, indRiesgo))
}

# Función auxiliar para obtener una fecha tipo dd/mm/aaaa de una fecha formato POSIXct

invertirFecha <- function (fecha){
	fecha <- as.character(fecha)
	año <- substr(fecha, 1, 4)
	mes <- substr(fecha, 6,7)
	dia <- substr(fecha, 9,10)
	fechaddmmaaaa <- paste(dia,"/",mes,"/",año, sep = "")

	return(fechaddmmaaa)
}

# Proceso para ejecutar el cálculo de Índice de Riesgo diario

# Preparación de archivo de logs
fecha <- Sys.Date()
sink(file = "Logs_ManchaNegra.txt", append = T, split = T)

mensajeLog <- paste(fecha, ": Inicio ejecución proceso", sep = "")
print(mensajeLog)

# Abrir archivo e iniciar proceso la variable manchaNegra tiene toda la
# información para ello
manchaNegra <- read.csv("CalculoManchaNegra.csv", header = T, as.is = T) # OJO SI SE PONEN MUCHOS E-MAILS RECOMENDABLE CAMBIAR ESTA MANERA DE ABRIR EL ARCHIVO POR
												# manchaNegra <- read.csv("CalculoManchaNegra.csv", skip = 1, header = F, as.is = T)
												# PARA NO COGER LA PRIMERA FILA Y NO TENER QUE INDICARLO EN LOS HEADERS
umbralRiesgo <- 0.5

mensajeLog <- paste(fecha, ": Hay ", nrow(manchaNegra)," estaciones a procesar", sep = "")
print(mensajeLog)

for(i in 1:nrow(manchaNegra)){

	# Inicializar variables
	alarma <- FALSE
	fechasRiesgo <- c()
	fechasProceso <- c()

	archivoDestino <- manchaNegra[i,2]
	archivoOrigen <- manchaNegra[i,1]
	

	# Extraer datos procesados
	datDestino <- read.csv(archivoDestino, header = T, as.is = T)
	estacionProcesada <- datDestino[nrow(datDestino),1]
	
	mensajeLog <- paste(fecha, ": Abierto archivo destino estacion ", estacionProcesada, sep = "")
	print(mensajeLog)

	if(archivoOrigen!="ARCIMIS"){
		datOrigen <- read.csv(archivoOrigen, skip = 4, header = F, as.is = T, na.strings = "NAN")
	} else{
		#aquí hay que hacer el procedimiento para que lea de la BBDD de ARCIMIS desde la
		#fecha que corresponda

		# que calcule los horarios

		# y que genere una dataframe con formato compatible con el resto de este procedimiento
	}
	
	ultDatDispnble <- as.POSIXct(datOrigen[nrow(datOrigen),1])
	ultDatCalc <- as.POSIXct(datDestino[nrow(datDestino),2])

	numDias <- floor(difftime(ultDatDispnble, ultDatCalc, units = "days"))

	# Realizar el cálculo del modelo para todos los días donde haya datos climáticos disponibles
	if(numDias<1){
		mensajeLog <- paste(fecha, ": No hay datos suficientes para calcular índice, estación de ", estacionProcesada, sep = "")
		print(mensajeLog)
	} else{
		
		extracto <- datOrigen[as.POSIXct(datOrigen[,1]) > ultDatCalc,]

		for(j in 1:numDias){
			mensajeLog <- paste(fecha, ": Procesado estación ", estacionProcesada, " día ", extracto[24*j,1], sep = "")
			print(mensajeLog)
			
			if(nrow(datDestino)>1) indRiesgoAyer <- datDestino[nrow(datDestino), 5] else indRiesgoAyer <- NA
			if(nrow(datDestino)>2) indRiesgoAntesAyer <- datDestino[nrow(datDestino)-1, 5] else indRiesgoAntesAyer <- NA		

			resultado <- calcularIndiceBSPCast(extracto[(1+(j-1)*24):(24*j),manchaNegra[i,3]], extracto[(1+(j-1)*24):(24*j),manchaNegra[i,4]])
			indRiesgoAcum <- resultado[3] + indRiesgoAyer + indRiesgoAntesAyer

			if(!is.na(indRiesgoAcum)){
				if(indRiesgoAcum > umbralRiesgo){
					Alarma <- TRUE
					fechasRiesgo[length(fechasRiesgo)+1] <- extracto[24*j,1]
					mensajeLog <- paste(fecha, ": Aparece riesgo dia ", extracto[24*j,1], " estacion ", estacionProcesada, sep = "")
					print(mensajeLog)
				}
			}
			fechasProceso[length(fechasProceso)+1] <- extracto[24*j,1]

			fila <- nrow(datDestino)+1
			datDestino[fila,1] <- datDestino[nrow(datDestino), 1]
			datDestino[fila,2] <- extracto[24*j,1]
			datDestino[fila,3] <- resultado[1]
			datDestino[fila,4] <- resultado[2]
			datDestino[fila,5] <- resultado[3]
			datDestino[fila,6] <- indRiesgoAcum
			#write.csv(datDestino[fila,1:6], archivoDestino, append = T, quote = F, row.names = F) ## (ALTERNATIVA NO EMPLEADA, SERÍA MÁS RÁPIDA YA QUE ES UN APPEND DE LA INFO GENERADA, NO DEL ARCHIVO COMPLETO, PERO NO FUNCIONA BIEN)
	
		}

		# Guardar archivo de resultados
		write.csv(datDestino, archivoDestino, quote = F, row.names = F)
		#write.csv(escribir, archivoDestino, append = T) ## (ALTERNATIVA NO EMPLEADA, SERÍA MÁS RÁPIDA YA QUE ES UN APPEND DE LA INFO GENERADA, NO DEL ARCHIVO COMPLETO, PERO NO FUNCIONA BIEN)
		mensajeLog <- paste(fecha, ": Fin procesado estación ", estacionProcesada, " datos guardados en ", archivoDestino, sep = "")
		print(mensajeLog)

		# Generar gráficos y guardarlos en PDF
		archivoSalida = paste(fecha, "_IndiceRiesgo-",estacionProcesada,".pdf", sep ="")
		pdf(archivoSalida)

		titulo <- paste("Indice riesgo Mancha Negra ", datDestino[nrow(datDestino), 1])
		limiteRiesgo <- rep (umbralRiesgo, length = length(datDestino$fecha)) 

		plot(as.POSIXct(datDestino$fecha), datDestino$IndRiesgoAcum, type = "l", ylim=range(c(0,1)), main = titulo, xlab = "Fecha", ylab = "Ind. Riesgo", col = "brown", lwd = 2)
		lines(as.POSIXct(datDestino$fecha),limiteRiesgo, col = "red", lwd = 2)
		legend(x = "topright", legend = c("Índice Riesgo Acumulado", "Umbral peligro infección"), col = c("brown", "red"), lty =c(1,1), lwd=c(2,2))

		dev.off()

		mensajeLog <- paste(fecha, ": Gráfico de riesgo generado para la estación ", estacionProcesada, " guardado como .PDF en ", archivoSalida, sep = "")
		print(mensajeLog)

		# Generar Subject y Body del e-mail
		# estudiar estas soluciones: http://stackoverflow.com/questions/3572607/sendmailr-part2-sending-files-as-mail-attachments?rq=1
		# protocolo MIME: http://www.freesoft.org/CIE/RFC/1521/index.htm

		if(alarma){
			subject <- paste("Se ha producido riesgo Mancha Negra estacion:", estacionProcesada, "detalles en el e-mail")
			if(length(fechasRiesgo)>1){
				body <- "De acuerdo al modelo BSPCast para el modelado de Mancha Negra en Peral se ha producido riesgo en las siguientes fechas: "
				for(z in 1:length(fechasRiesgo)) body <- paste(body, invertirFecha(fechasRiesgo[z]), sep = "\n")
			}
			else{
				body <- paste("De acuerdo al modelo BSPCast para el modelado de Mancha Negra en Peral se ha producido riesgo el día:",fechasRiesgo[1])
			}
		} else{
			subject <- paste("Resultados riesgo Mancha negra estacion:", estacionProcesada)
			body <- "De acuerdo al modelo BSPCast para el modelado de Mancha Negra en Peral no se ha producido riesgo."
		}

		body <- paste(body,"","","Fechas procesadas:","", sep="\n")
		for(z in 1:length(fechasProceso)) body <- paste(body,invertirFecha(fechasProceso[z]), sep="\n")

		body <- paste(body, "","", sep = "\n")
		body <- paste(body, "Aplicaciones experimentales", "Servicio de Información Agroclimática de La Rioja", "SIDTA-CIDA", sep = "\n")
		body <- paste(body, "Ctra. Mendavia-Logroño NA-134, km. 90", "26071 Logroño","Tlfno: 941 29 18 34", "Fax: 941 29 13 92", "correo-e: siar.cida@larioja.org", sep ="\n")
		body <- paste(body, "","", sep = "\n")
		body <- paste(body, "Ha recibido este mensaje al aparecer en nuestra bases de datos como usuario que desea recibir alguno de nuestros servicios ", sep = "")
		body <- paste(body, "experimentales. Estos servicios están en desarrollo y pueden presentar problemas de funcionamiento por lo que agradecemos ", sep ="")
		body <- paste(body, "cualquier sugerencia que realice para mejorarlos. Para más información póngase en contacto con nosotros usando cualquiera ", sep="")
		body <- paste(body, "de las formas de contacto que aparecen en la firma del e-mail. Si ha recibido este mensaje por error o no desea recibir más ", sep="")
		body <- paste(body, "correos de este tipo, le pedimos disculpas y le rogamos que nos lo comunique a través del e-mail que aparece en la firma ", sep="")
		body <- paste(body, "para proceder a anular su suscripción.", sep ="")
		body <- paste(body, "", "Muchas gracias por su colaboración", "", sep ="\n")
		body <- paste(body, "Nota: La dirección labsiar@larioja.org no es leída por ningún integrante del equipo de trabajo del SIAR, por favor no la use ", sep="\n")
		body <- paste(body, "para intentar comunicar con nosotros. Si pulsa el botón responder cambie el destinatario por siar.rioja@larioja.org.", sep = "")



		msg=list(mime_part(body), mime_part(archivoSalida))

		cat("[1] ", as.character(fecha), ": Enviados e-mails a: ")
		for(to in manchaNegra[i,6:ncol(manchaNegra[i,])]){
			if(!is.na(to)){
				resultadoEmail <- sendmail(from,to,subject,msg, control=list(smtpServer="correo6.larioja.org"))
				cat(to, " ", resultadoEmail$code, " ", resultadoEmail$msg,", ")
			}
		}
		cat("\n")

		
	} # cierre de la llave del else
	cat("[1] ", as.character(fecha), ": Fin procesado estación ", estacionProcesada)
	cat("\n")
} # cierre de la llave del for

mensajeLog <- paste(fecha, ": Fin ejecución proceso", sep = "")
print(mensajeLog)
sink()




