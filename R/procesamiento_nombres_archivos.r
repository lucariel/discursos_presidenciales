#procesamiento_nombres_archivos.r: por ahora aca van mini scripts, luego evolucionar a funciones / archivos
library(here)
library(tidyverse)
path_original <- here::here("data","sesiones_ordinarias")
listado_nombres <- grep(list.files(path=path_original), pattern='.txt$', inv=TRUE, value=TRUE) %>% 
    tibble::enframe(name = "orden_archivos_original",value = "nombre_archivo") %>%
    dplyr::mutate(anio_old=stringr::str_extract(nombre_archivo,pattern = rebus::one_or_more(rebus::DGT ))) %>% 
    dplyr::mutate(alias_old=stringr::str_replace(nombre_archivo,pattern = anio_old,replacement = "")) %>% 
    dplyr::mutate(anio=if_else(stringr::str_length(anio_old)==2,paste0("19",anio_old),anio_old)) %>%
    dplyr::mutate(anio=if_else(alias_old=="delarua",paste0("20",anio_old),anio)) %>% 
    # extraido de wikipedia a futuro se podria scrappear esto.
    dplyr::mutate(nombre_apellido=case_when(
        alias_old=="juarez celman "~"Miguel Juárez Celman",
        alias_old=="alfonsin"~"Raúl Alfonsín",
        alias_old=="alvear"~"Marcelo Torcuato de Alvear",
        alias_old=="delarua"~"Fernando de la Rúa",
        alias_old=="duhalde"~"Eduardo Duhalde",
        alias_old=="nk"~"Néstor Kirchner",
        alias_old=="cfk"~"Cristina Fernández de Kirchner",
        alias_old=="macri"~"Mauricio Macri",
        alias_old=="estelaPeron"~"María Estela Martínez de Perón",
        alias_old=="frondizi"~"Arturo Frondizi",
        alias_old=="illia"~"Arturo Illia",
        alias_old=="peron"~"Juan Domingo Perón",
        alias_old=="menem"~"Carlos Menem",
        TRUE~NA_character_
    )) %>% 
    # extraido de janitor::clean_names
    dplyr::mutate(nombre_apellido_file=snakecase::to_snake_case(nombre_apellido)) %>% 
    dplyr::mutate(nombre_apellido_file=snakecase::to_any_case(nombre_apellido_file,
                                                              case = "snake", sep_in = "\\.", 
                                                              transliterations = c("Latin-ASCII"), 
                                                              parsing_option = 4)) %>% 
    dplyr::mutate(nuevo_nombre=paste0(anio,"-",nombre_apellido_file,".txt"))

if (nrow(listado_nombres)>0) {
    listado_nombres %>% 
        dplyr::mutate(resultado_renombre=purrr::map2(.x = paste0(path_original,"/",nombre_archivo),
                                                     .y = paste0(path_original,"/",nuevo_nombre),
                                                     .f = base::file.copy))
    
    listado_nombres %>% 
        dplyr::mutate(resultado_borrar=purrr::map(.x= paste0(path_original,"/",nombre_archivo),
                                                  .f=file.remove)) 
}
        
