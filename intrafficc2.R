#Codigo

#  Paquetes requeridos
 
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)

# Lee los archivos detektor.csv , report.csv, rtl.csv y tvl.csv en la carpeta producción

wd <- getwd()

setwd( paste0( wd,"/producción" ) )
files_produc <- list.files( full.names = TRUE )
tar_logical_produc <- grepl( ".tar" , files_produc )
sapply( files_produc[ tar_logical_produc ], untar )
files_produc <- list.files( full.names = TRUE )
tar_logical_produc <- grepl( ".tar" , files_produc )
sapply( files_produc[ tar_logical_produc ] , file.remove )

all_prod <- mapply( read.csv , 
                    files_produc,
                    header = FALSE ) 


#  Une los archivos .csv en producción según su clase (detektor, report, rtl, tvl)

detektorCombinProduc <- rbind.data.frame( all_prod[[ 2 ]] , all_prod[[ 1 ]])
reportCombinProduc <- rbind.data.frame( all_prod[[ 4 ]] , all_prod[[ 3 ]] )
rtlCombinProduc <- rbind.data.frame( all_prod[[ 6 ]] , all_prod[[ 5 ]] ) 
tvlCombinProduc <- rbind.data.frame( all_prod[[ 8 ]] , all_prod[[ 7 ]] ) 

combinProduc <- list( detektorCombinProduc ,
                      reportCombinProduc ,
                      rtlCombinProduc ,
                      tvlCombinProduc )
combinProducNames<-c("detektorCombinProduc.csv",
                      "reportCombinProduc.csv",
                      "rtlCombinProduc.csv" ,
                      "tvlCombinProduc.csv")

# Crea directorio CombinTestinge incluye los archivos csv combinados en el
setwd(wd)
dir.create( "CombinProduc" )
setwd( paste0( wd, "/CombinProduc" ) )

mapply(write.csv,combinProduc,combinProducNames)

# Lee los archivos detektor.csv , report.csv, rtl.csv y tvl.csv en la carpeta testing de la misma manera que se hizo en producción

setwd( paste0( wd , "/testing" ) )
files_test <- list.files( full.names = TRUE )

tar_logical_test<-grepl(".tar",files_test)
sapply(files_produc[tar_logical_test], untar)
files_test<-list.files(full.names = TRUE)
tar_logical_test<-grepl(".tar",files_test)
sapply(files_produc[tar_logical_test], file.remove)
all_test <- mapply( read.csv,
                    files_test,
                    header = FALSE )

#  Une los archivos .csv en testing según su clase (detektor, report, rtl, tvl), de esta manera crea las tablas de datos detektorCombinTesting, reportCombinTesting, rtlCombinTesting, tvlCombinTesting.

detektorCombinTesting <- rbind.data.frame( all_test[[ 2 ]] , all_test[[ 1 ]])
reportCombinTesting <- rbind.data.frame( all_test[[ 5 ]] ,
                                         all_test[[ 4 ]] ,
                                         all_test[[3]])
rtlCombinTesting <- rbind.data.frame( all_test[[ 7 ]] , all_test[[ 6 ]] ) 
tvlCombinTesting <- rbind.data.frame( all_test[[ 8 ]] , all_test[[ 9 ]] ) 

#Crea listas de combinProduct y combinTtesting

combinTesting <- list( detektorCombinTesting ,
                       reportCombinTesting ,
                       rtlCombinTesting ,
                       tvlCombinTesting )
combinTestingNames<-c("detektorCombinTesting.csv",
                      "reportCombinTesting.csv",
                      "rtlCombinTesting.csv" ,
                      "tvlCombinTesting.csv")
# Crea directorio CombinTesting e incluye los archivos csv combinados en el
setwd(wd)
dir.create( "CombinTesting" )
setwd( paste0( wd, "/CombinTesting" ) )

mapply(write.csv,combinTesting,combinTestingNames)


# Identifica si los archivos Detektor combinados en testing y producción son identicos, si no, busca la fila a partir de la cual son iguales

match_prod_test <- vector( length = 4 )
mensaje <- vector( length = 4 )
if ( ncol( detektorCombinProduc ) == ncol( detektorCombinTesting ) ) {
        compare_dim_detektor <- ( nrow( detektorCombinProduc ) == nrow( detektorCombinTesting ) )
        if ( compare_dim_detektor == FALSE ) {
                match_prod_test[ 1 ] <- match( detektorCombinTesting[ 1 , 4 ] ,
                                               detektorCombinProduc$V4 )
                mensaje[ 1 ] <- paste( "los archivos Detektor son distintos, " ,
                                       "los archivos son iguales a partir de la fila: ",
                                       match_prod_test[ 1 ] )
        }
        if ( compare_dim_detektor == TRUE ) {
                mensaje[ 1 ] <- "los archivos Detektor son iguales"
        }
}

# Identifica si los archivos report, rtl y tvl combinados en produccion y testing son identicos, si no, busca la fila a partir de la cual son iguales

if ( ncol( reportCombinProduc ) == ncol( reportCombinTesting ) ) {
        compare_dim_report <- ( nrow( reportCombinProduc) == nrow( reportCombinTesting ) )
        if ( compare_dim_report == FALSE ) {
                match_prod_test[ 2 ] <- match( reportCombinTesting[ 1 , 9 ] ,
                                             reportCombinProduc$V9 )
        }
        mensaje[ 2 ] <- paste( "los archivos Reportes son distintos, " ,
                           "los archivos son iguales a partir de la fila: ",
                           match_prod_test[ 2 ] )
        if ( compare_dim_report == TRUE ) {
        mensaje[ 2 ] <- "las dimensiones de los archivos Reportes son iguales"
        }
}

if ( ncol( rtlCombinProduc ) == ncol( rtlCombinTesting ) ) {
        compare_dim_rtl <- ( nrow( rtlCombinProduc ) == nrow( rtlCombinTesting ) )
        if ( compare_dim_rtl == FALSE ) {
        match_prod_test[ 3 ] <- match( rtlCombinTesting[ 1 , 5 ] ,
                                     rtlCombinProduc$V5 )
        mensaje[ 3 ] <- paste( "los archivos rtl son distintos, " , 
                           "los archivos son iguales a partir de la fila: " , 
                           match_prod_test[ 3 ] )
        }
        if (compare_dim_rtl==TRUE) {
                mensaje[ 3 ] <- "las dimensiones de los archivos rtl son iguales"
        }
}

if ( ncol( tvlCombinProduc ) == ncol( tvlCombinTesting ) ) {
        compare_dim_tvl <- ( nrow( tvlCombinProduc ) == nrow( tvlCombinTesting ) )
        if ( compare_dim_tvl == FALSE ) {
                match_prod_test[ 4 ] <- match( tvlCombinTesting[ 1 , 5 ] ,
                                               tvlCombinProduc$V5 )
                mensaje[ 4 ] <- paste( "los archivos tvl son distintos, " ,
                                   "los archivos son iguales a partir de la fila: " , 
                                   match_prod_test[ 4 ] )
        }
        if ( compare_dim_tvl == TRUE ) {
                mensaje[ 4 ] <- "lass dimensiones de los archivos tvl son iguales"
        }
}


# Crea directorio Cropeado incluye los archivos csv Cropeados en el
setwd(wd)
dir.create( "Cropeado" )
setwd( paste0( wd, "/Cropeado") )

# Crea los archivos reportCombinCropeadoProduc, rtlCombinCropeadoProduc y tvlCombinCropeadoProduc

archivosCropeados <- c( "detektorCombinCropeadoProduc.csv" ,
                        "reportCombinCropeadoProduc.csv" ,
                        "rtlCombinCropeadoProduc.csv" ,
                        "tvlCombinCropeadoProduc.csv" )

archivosCropeados_data<-list()

match_mensaje <- vector( length = 4 )

for ( i in c( 1 : 4 ) ) {
        if ( is.na( match_prod_test[ i ] ) == TRUE ) {
                match_prod_test_alternativo<-vector( length = ncol( combinTesting[[i]] ) )
                for ( j in c( 1:ncol( combinTesting[[i]] ) ) )  {
                        match_prod_test_alternativo[ j ] <- match( combinTesting[[i]][ 1 , j ] ,
                                                                   combinProduc[[i]][,j] )
                }
                match_mensaje[ i ] <- paste( "No hay ningun N+1 a partir del cual se pueda constuir ",
                                             archivosCropeados[ i ],
                                             ", los archivos tienen datos iguales en las filas",as.data.frame(match_prod_test_alternativo), "en las columnas correspondientes")
                
        }
        if ( is.na( match_prod_test[ i ] ) == FALSE ) {
                
                match_mensaje[ i ] <- mensaje[ i ]
                archivosCropeados_data[[ i ]] <- combinProduc[[ i ]][ -( 1 : match_prod_test[ i ] - 1 ) , ]
                write.csv( archivosCropeados_data[[ i ]] , archivosCropeados[ i ] )
        }

}
setwd( wd )
print( match_mensaje )

# elimina filas repetidas en los archivos cropeados y testing

archivosCropeados_data <- lapply( archivosCropeados_data,unique )

combinTesting <- lapply( combinTesting , unique )

unidos_cropeados_detektor <- unite( archivosCropeados_data[[ 1 ]] ,
                                    V2 , V2 , V3 , V4 , V5 , V6 , V7 , V8 , V9 ,
                                    sep = "" ) %>% select( V2 )

#une columnas en produccion y testing para comparar datos 

unidos_Testing_detektor <- unite( combinTesting[[ 1 ]] ,
                                  V2 , V2 , V3 , V4 ,V5 , V6 , V7 , V8 , V9 ,
                                  sep = "" ) %>% select( V2 )

unidos_cropeados_reportes <- unite( archivosCropeados_data[[ 2 ]] ,
                                  V1 , V1 , V2 , V3 , V4 , V5 , V6 , V7 , V8 , V9 , V10 , V11 , V12 , V13 ,
                                  sep = "" ) %>% select(V1)

unidos_Testing_reportes <- unite( combinTesting[[ 2 ]] ,
                                V1 , V1 , V2 , V3 , V4 , V5 , V6 , V7 , V8 , V9 , V10 , V11 , V12 , V13 ,
                                sep = "" ) %>% select( V1 )

#los elementos false son los que no se consiguieron en cropeado

match_testingCropeado_detektor <- !is.na( match( unidos_Testing_detektor[ , 1 ] ,
                                                 unidos_cropeados_detektor[ , 1 ] ) 
                                          )

match_testingCropeado_reportes <- !is.na( match( unidos_Testing_reportes[ , 1 ] ,
                                          unidos_cropeados_reportes[ , 1 ] ) 
                                          )
# No se pudo cropear rtl y tvl por lo que se hace un estudio de coincidencias 

#rtl
 
coincidencia_rtl <- mapply( match , rtlCombinTesting , rtlCombinProduc )
coin_data_log_rtl <- matrix(ncol = ncol(rtlCombinProduc),nrow = nrow(coincidencia_rtl))
rtlCombinProduc <- mutate( rtlCombinProduc , V7 = date( V7 ) )
rtlCombinTesting <- mutate( rtlCombinTesting , V7 = date( V7 ) )

for (i in c( 1 : ncol( rtlCombinProduc ) ) ) {
        coin_data_log_rtl[ , i ] <- rtlCombinProduc[ coincidencia_rtl[ , 1 ] , i ] == rtlCombinTesting[ , i ]
}

coin_data_log_rtl <- cbind( coincidencia_rtl[ , 1 ] , coin_data_log_rtl )
sum_coin_data_log_rtl <- apply( coin_data_log_rtl[ , -1 ] , 1 , sum )

coin_data_rtl <- cbind( coincidencia_rtl[ , 1 ] , sum_coin_data_log_rtl ) %>% 
        as.data.frame() %>% `colnames<-`( c( "posición" , "número de coincidencias" ) )

#tvl

coincidencia_tvl <- mapply( match , tvlCombinTesting , tvlCombinProduc )
coin_data_log_tvl <- matrix( ncol = ncol( tvlCombinProduc ) , nrow = nrow( coincidencia_tvl ) )
tvlCombinProduc <- mutate( tvlCombinProduc , V7 = date( V7 ) )
tvlCombinTesting <- mutate( tvlCombinTesting , V7 = date( V7 ) )

for (i in c( 1 : ncol( tvlCombinProduc ) ) ) {
        coin_data_log_tvl[ , i ] <- tvlCombinProduc[ coincidencia_tvl[ , 1 ] , i ] == tvlCombinTesting[ , i ]
}

coin_data_log_tvl <- cbind( coincidencia_tvl[ , 1 ] , coin_data_log_tvl )
sum_coin_data_log_tvl <- apply( coin_data_log_tvl[ , -1 ] , 1 , sum )

coin_data_tvl <- cbind( coincidencia_tvl[ , 1 ] , sum_coin_data_log_tvl ) %>% 
        as.data.frame() %>% `colnames<-`( c( "posición" , "número de coincidencias" ) )

#en esta sección se crea gráfico descriptivo de los resultados 

dir.create( "statistical_plots" )
setwd( paste0( wd , "/statistical_Plots" ) )

nrows_Cropeado <- unlist( mapply( nrow , archivosCropeados_data ) )
nrows_produc <- mapply( nrow , combinProduc )[ -c(3:4) ]
nrows_testing<-mapply(nrow,combinTesting)[-c(3:4)]
nrows_data <- rbind( nrows_produc , nrows_Cropeado ) %>% 
        data.frame() %>% 
        setNames( c( "detektor" , "report" ) ) %>%
        gather( file , nrows )
nrows_data2<-rbind( nrows_testing , nrows_Cropeado ) %>% 
        data.frame() %>% 
        setNames( c( "detektor" , "report"  ) ) %>%
        gather( file , nrows )
new_column <- paste( nrows_data$file , c( "produc" , "cropeado" ) )
new_column2 <- paste( nrows_data2$file , c( "testing" , "cropeado" ) )
nrows_data <- mutate( nrows_data,file = new_column )
nrows_data2 <- mutate( nrows_data2,file = new_column2 )

png("barplot.png",width = 600 )
print(ggplot( nrows_data , aes( file , nrows ) ) +
              geom_bar( stat = "identity" , fill= "blue", alpha=.6) +
              geom_text( aes( label = nrows ) , vjust = 1.6 , color = "white", size = 3.5 ) +
              theme_minimal())
dev.off()
png("poduc_testing.png",width = 600)
print(ggplot( nrows_data2 , aes( file , nrows ) ) +
              geom_bar( stat = "identity" , fill= "blue", alpha=.6) +
              geom_text( aes( label = nrows ) , vjust = 1.6 , color = "white", size = 3.5 ) +
              theme_minimal())
dev.off()
setwd(wd)
