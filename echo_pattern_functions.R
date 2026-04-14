# Carga la librería vegan necesaria para la función mantel
library(vegan)

# Argumentos:
#   skeleton_1: matriz de tamaño Nx2 con las coordenadas (X, Y) de los puntos 
#               del primer esqueleto (obtenido de OpenPose o dfMaker)
#   skeleton_2: matriz de tamaño Nx2 con las coordenadas del segundo esqueleto
#               (El orden de los esqueletos no afecta al resultado)
#   points_to_compare: vector con los índices de los puntos que se quieren comparar
# Salida:
#   Devuelve un vector con dos valores:
#     r → estadístico de correlación de Mantel (entre -1 y 1)
#     p → valor p asociado (nivel de significación)




# echo_pattern <- function(frame_1, frame_2, people_id_1, people_id_2, points_to_compare, video_dt){
#   
# 
#   
#   skeleton_1 <- video_dt[(frame == frame_1 & people_id == people_id_1), .(nx, ny)]
#   skeleton_2 <- video_dt[(frame == frame_2 & people_id == people_id_2), .(nx, ny)]
#   
#   
#   # Calcula las matrices de distancias euclídeas entre los puntos seleccionados 
#   M_1 <- dist(skeleton_1[points_to_compare, ], method = "euclidean")
#   M_2 <- dist(skeleton_2[points_to_compare, ], method = "euclidean")
#   
#   # Realiza la prueba de Mantel para comparar las dos matrices de distancia
#   result <- mantel(M_1, M_2, method = "pearson", permutations = 999)
#   
#   # Devuelve el coeficiente de correlación (r) y el valor p
#   return(c(r = result$statistic, p = result$signif))
#   
# }
# 
# VERSIÓN VIEJA





# ESTA CALCULA MANTEL ENTRE AMBAS MATRICES (Mide si la estructura de distancias entre elementos de las matrices se parecen)
echo_pattern <- function(frame_1, frame_2, people_id_1, people_id_2, video_dt, typepoints){
  
  if (!(typepoints %in% c("pose_keypoints", "face_keypoints", "hand_left_keypoints", "hand_right_keypoints", "all_points"))) {
    print(paste("Has elegido los typepoints", typepoints))
    stop("typepoints debe ser pose_keypoints, face_keypoints, hand_left_keypoints, hand_right_keypoints o all_points")
  }
  
  
  if (typepoints == "all_points"){
    skeleton_1 <- video_dt[
      frame == frame_1 & people_id == people_id_1,
      .(nx, ny)
    ]
    
    skeleton_2 <- video_dt[
      frame == frame_2 & people_id == people_id_2,
      .(nx, ny)
    ]
  }
  else {
    skeleton_1 <- video_dt[
      frame == frame_1 & people_id == people_id_1 & type_points == typepoints,
      .(nx, ny)
    ]
    
    skeleton_2 <- video_dt[
      frame == frame_2 & people_id == people_id_2 & type_points == typepoints,
      .(nx, ny)
    ]
    
  }
  
  
  
  
  
  # Eliminar filas con NA en cualquiera de los dos esqueletos
  valid_rows <- is.finite(skeleton_1$nx) & is.finite(skeleton_1$ny) &
    is.finite(skeleton_2$nx) & is.finite(skeleton_2$ny)
  
  skeleton_1 <- skeleton_1[valid_rows, ]
  skeleton_2 <- skeleton_2[valid_rows, ]
  
  # Comprobar que quedan suficientes puntos
  if (nrow(skeleton_1) < 3 || nrow(skeleton_2) < 3) {
    return(c(r = NA_real_, p = NA_real_))
  }
  
  # Matrices de distancia
  M_1 <- dist(skeleton_1, method = "euclidean")
  M_2 <- dist(skeleton_2, method = "euclidean")
  
  # Prueba de Mantel
  result <- mantel(M_1, M_2, method = "pearson", permutations = 999)
  
  return(c(r = unname(result$statistic), p = result$signif))
}



# ESTA CALCULA LA DISTANCIA EUCLIDEANA ENTRE AMBAS MATRICES (DIFERENCIA ENTRE MATRICES)
echo_pattern_eucdis <- function(frame_1, frame_2, people_id_1, people_id_2, video_dt, typepoints){
  
  if (!(typepoints %in% c("pose_keypoints", "face_keypoints", "hand_left_keypoints", "hand_right_keypoints", "all_points"))) {
    print(paste("Has elegido los typepoints", typepoints))
    stop("typepoints debe ser pose_keypoints, face_keypoints, hand_left_keypoints, hand_right_keypoints o all_points")
  }
  
  if (typepoints == "all_points"){
    skeleton_1 <- video_dt[
      frame == frame_1 & people_id == people_id_1,
      .(nx, ny)
    ]
    
    skeleton_2 <- video_dt[
      frame == frame_2 & people_id == people_id_2,
      .(nx, ny)
    ]
  } else {
    skeleton_1 <- video_dt[
      frame == frame_1 & people_id == people_id_1 & type_points == typepoints,
      .(nx, ny)
    ]
    
    skeleton_2 <- video_dt[
      frame == frame_2 & people_id == people_id_2 & type_points == typepoints,
      .(nx, ny)
    ]
  }
  
  # Comprobar que ambos tienen el mismo número de puntos
  if (nrow(skeleton_1) != nrow(skeleton_2)) {
    warning("Los esqueletos no tienen el mismo número de puntos")
    return(c(distance = NA_real_))
  }
  
  # Eliminar filas con NA en cualquiera de los dos esqueletos
  valid_rows <- is.finite(skeleton_1$nx) & is.finite(skeleton_1$ny) &
    is.finite(skeleton_2$nx) & is.finite(skeleton_2$ny)
  
  skeleton_1 <- skeleton_1[valid_rows, ]
  skeleton_2 <- skeleton_2[valid_rows, ]
  
  # Comprobar que quedan suficientes puntos
  if (nrow(skeleton_1) < 3 || nrow(skeleton_2) < 3) {
    return(c(distance = NA_real_))
  }
  
  # Distancia euclídea punto a punto
  point_distances <- sqrt((skeleton_1$nx - skeleton_2$nx)^2 +
                            (skeleton_1$ny - skeleton_2$ny)^2)
  
  # Distancia media
  mean_distance <- mean(point_distances)
  
  return(c(distancia = mean_distance))
}