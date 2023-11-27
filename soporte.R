# Librerías ----
pacman::p_load(gt, tidyverse, funModeling, googlesheets4, gargle, 
               extrafont, scales, ggalt, kableExtra, wordcloud, networkD3,
               data.table, ggeconodist)

# Configuraciones generales ----
options(scipen = 999) # Modifica la visualización de los ejes numérico a valores nominales

loadfonts(quiet = TRUE) # Permite cargar en R otros tipos de fuentes.

# Estilo limpio sin líneas de fondo
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#FBFCFC"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia horizontales en gris claro
estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.y = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris
genero3 <- c("#8624F5","#FFD129", "#1FC3AA")

colores <-  c("#8624F5", "#1FC3AA")

azul <- "#344D7E"
verde <-  "#1FC3AA"
rosa1 <- "#B95192"
rosa2 <- "#EE5777"
naranja <- "#FF764C"
amarillo <- "#FFD129"
gris <- "#75838F"
lila <- "#8624F5"
rojo <- "#943126"

col4 <- c(azul, lila, rosa1, rosa2)
col5 <- c(azul, lila, rosa1, rosa2, naranja)
col6 <- c(azul, lila, rosa1, rosa2, naranja, amarillo)

# Creo un objeto con un texto que se va a repetir mucho a lo largo del análisis
fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam 2023"

# Creo objetos para formatear las etiquetas numéricas de los ejes x e y
eje_x_n <- scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

eje_y_n <- scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


# Datos 2023 -----

# Encuesta 
kiwi23 <- read_sheet("1ZICEdPiduO0cE13MtWF0E0UeOz72TtH3_162pqtgeUo") %>% 
  janitor::clean_names()

# Tipo de cambio 2023
tc <- read_sheet("1fIyvCeHsljbu4Fatjsp0_KV0TZgmJMncJQoXmrkpHGI") %>% 
  janitor::clean_names()

# Limpieza de datos 2023 -------

# Añade columna de id
kiwi23$id <- rep(1:nrow(kiwi23))

# Eliminar preguntas
kiwi23 <- kiwi23 %>% 
  select(id, everything(),
         -queres_contestar_mas_preguntas_28,
         -queres_contestar_mas_preguntas_37,
         -queres_contestar_mas_preguntas_45,
         -comparti_tu_meme_de_rrhh_favorito,
         -comparti_tu_meme_favorito_de_rrhh_o_relacionado_con_tu_trabajo,
         -comentarios)

# Renombrar columnas
kiwi23 <- kiwi23 %>% 
  rename(fecha = marca_temporal,
         genero = identidad_de_genero,
         nivel_formacion = maximo_nivel_de_formacion,
         carrera_grado = que_carrera_de_grado_estudiaste,
         tipo_universidad = en_que_tipo_de_universidad_estudiaste_tu_carrera_de_grado,
         pais = pais_en_el_que_trabajas,
         provincia = provincia_donde_trabajas,
         rubro = rubro_de_la_empresa,
         dotacion = cuantos_empleados_tiene_la_empresa,
         origen_capital = origen_del_capital,
         dotacion_rh = cuantas_personas_integran_el_area_de_rrhh,
         puesto = en_que_puesto_trabajas,
         tipo_contratacion = tipo_de_contratacion,
         jornada = como_es_tu_jornada_laboral,
         funcion = cual_es_tu_funcion_principal_en_rrhh,
         personas_a_cargo = cuantas_personas_tenes_a_cargo_pone_0_si_no_tenes_gente_a_cargo,
         anios_empresa = hace_cuantos_anos_trabajas_en_la_empresa_donde_estas_0_para_menos_de_un_ano,
         anios_puesto = hace_cuantos_anos_estas_en_tu_puesto_actual_0_para_menos_de_un_ano,
         anios_rh = cuantos_anos_de_experiencia_tenes_en_rrhh,
         sueldo_bruto = cual_es_tu_remuneracion_bruta_mensual_en_tu_moneda_local_antes_de_impuestos_y_deducciones,
         modalidad = en_que_modalidad_estas_trabajando_actualmente,
         beneficios = que_beneficios_tenes,
         bono = recibis_bonos,
         ajuste = tuviste_ajustes_por_inflacion_en_2023,
         ajuste_porcentaje = cual_fue_el_porcentaje_de_aumento_acumulado_que_tuviste_en_2023,
         idioma_exigencia = te_exigieron_saber_un_idioma_extranjero_ingles_portugues_etc_para_entrar_a_trabajar_en_tu_empresa,
         idioma_situaciones = en_que_situaciones_usas_un_idioma_extranjero,
         recomendacion = recomendaria_la_empresa_para_trabajar_a_un_amigo_o_amiga,
         busqueda = estas_buscando_trabajo,
         comunidad = participas_en_alguna_comunidad_o_voluntariado_de_rrhh,
         comunidades = en_cuales,
         consejo_empleados = que_consejo_le_darias_a_alguien_que_quiera_empezar_a_estudiar_o_trabajar_en_rrhh,
         diversidad_sexual = te_identificas_como_lgbtiq_lesbiana_gay_bisexual_transexual_otra_minoria_sexual,
         discapacidad = tenes_alguna_discapacidad,
         diversidad_management = que_porcentaje_aproximado_del_management_de_tu_empresa_son_mujeres_entiendase_posiciones_de_jefatura_de_gerencia_o_de_direccion,
         contrata_senior = en_lo_que_va_del_ano_han_contratado_en_tu_empresa_a_personas_mayores_de_50_anos,
         contrata_discapacidad = en_lo_que_va_del_ano_han_contratado_en_tu_empresa_a_personas_con_discapacidad,
         sufrio_acoso = sufriste_alguna_situacion_de_acoso_abuso_o_de_discriminacion_en_algun_trabajo,
         machismo = sentis_que_tu_entorno_laboral_es_machista,
         procesos_erp = que_procesos_gestionan_a_traves_de_algun_sistema_de_gestion,
         tienen_pa = tienen_un_area_de_people_analytics_en_la_empresa,
         herramientas_analisis = que_herramientas_usan_en_rrhh_para_hacer_analisis,
         prioridad_pa = durante_2023_la_importancia_de_people_analytics_dentro_de_las_prioridades_del_area,
         usa_genai = utilizas_alguna_plataforma_de_ia_generativa_como_chat_gpt_o_bard_aplicada_a_casos_de_rrhh,
         genai_casos = para_que_has_usado_chat_gpt_en_rrhh_o_una_herramienta_similar,
         capacitacion_pa = durante_el_presente_ano_y_dentro_de_rrhh_han_realizado_alguna_capacitacion_relacionada_con_los_siguientes_temas,
         registro_fiscal = como_estas_registrado_a_fiscalmente,
         anios_freelance = hace_cuantos_anos_trabajas_como_freelance_0_para_menos_de_un_ano,
         motivo = que_te_motivo_a_trabajar_por_tu_cuenta,
         lugar_trabajo = donde_trabajas_habitualmente,
         exporta = exportas_tus_servicios,
         medio_pago_exterior = si_exportas_servicios_a_traves_de_que_medios_de_pago_recibis_los_pagos_del_exterior,
         cuotas = aceptas_pagos_en_cuotas,
         colaboracion_freelance = trabajas_con_otros_freelancers_de_tu_mismo_rubro,
         servicio_busqueda = tu_servicio_principal_esta_relacionado_con_busqueda_y_seleccion,
         busqueda_it = te_dedicas_principalmente_a_realizar_busquedas_de_it_tecnologia,
         entrevista_ingles = haces_entrevistas_en_ingles,
         riesgo = trabajas_a_riesgo,
         ingresos_fijo_variable = tus_ingresos_son_fijos_o_variables_comision_por_busqueda_cerrada,
         coeficiente = cual_es_el_coeficiente_que_cobras_por_tus_servicios,
         base_coeficiente = el_coeficiente_lo_calculas_sobre,
         garantia = ofreces_garantia,
         busqueda_tiempo = cual_es_el_tiempo_promedio_aproximado_para_cerrar_busquedas_en_dias,
         busqueda_barrera = cuales_son_las_principales_dificultades_que_encontras_a_la_hora_de_cerrar_una_busqueda,
         requisitos_raros = cual_fue_el_requisito_mas_extrano_que_te_ha_pedido_algun_postulante,
         servicio_principal = cual_es_el_servicio_principal_que_brindas_si_brindas_mas_de_un_servicio_elegi_el_que_mas_ingresos_genere,
         modalidad_servicios = bajo_que_modalidad_prestas_tus_servicios,
         negociacion = te_cuesta_mucho_definir_el_precio_con_nuevos_clientes,
         valor_hora = cual_es_el_valor_hora_promedio_que_ofreces_moneda_local
         )

# Base de empleados en relación de dependencia ----
rh23 <- kiwi23 %>% 
  filter(trabajo == "Relación de Dependencia") %>% 
  select(id:capacitacion_pa)

# Limpiar columnas en formato lista
rh23 <- rh23 %>% 
  mutate(dotacion_rh = as.numeric(unlist(dotacion_rh)),
         anios_rh = as.numeric(unlist(anios_rh)),
         ajuste_porcentaje = as.numeric(unlist(ajuste_porcentaje)))

## Añade datos de tipo de cambio a dolar ----
rh23 <- rh23 %>% 
  left_join(tc, by = "pais") %>% 
  mutate(multiplicador = if_else(tipo_contratacion == "Part time", 1.5, 1),
         sueldo_ft = sueldo_bruto * multiplicador,   # Hace la equivalencia de un sueldo part time a full time
         sueldo_dolar = sueldo_ft/tipo_cambio,       # Convierto los sueldos a dólares
         cuenta = 1)

# Guarda csv rh23
write_delim(rh23, file = "data/rh_2023.csv",
            delim = ";")


###########################################
# Creación de dataframe Freelancers -----

freelo23 <- kiwi23 %>% 
  filter(trabajo == "Freelance")

# Eliminar columnas vacias
freelo23 <- freelo23 %>% 
  select(where (~ !all(is.na(.x))), # Elimina columnas con valores NA
         -trabajo, 
         -dotacion_rh,
         -anios_rh,
         -ajuste_porcentaje,
         -comunidades) 

# Convertir las columnas tipo lista
freelo23 <- freelo23 %>%
  mutate(anios_freelance = as.numeric(unlist(anios_freelance)))

freelo23$motivo[[29]] <- "No es por motivación. Usualmente el vinculo laboral para posiciones de consultoría en Uruguay son de 'independencia'."
freelo23$motivo[[24]] <- as.character(freelo23$motivo[[24]])


freelo23 <- unnest(data = freelo23, cols = motivo, keep_empty = TRUE)

# Corregir coeficientes de recruiters
freelo23$coeficiente[[8]] <- as.numeric(freelo23$coeficiente[[8]])
freelo23$coeficiente[[12]] <- as.numeric(freelo23$coeficiente[[12]])
freelo23$coeficiente[[14]] <- as.numeric(freelo23$coeficiente[[14]])
freelo23$coeficiente[[15]] <- as.numeric(freelo23$coeficiente[[15]])
freelo23$coeficiente[[16]] <- as.numeric(freelo23$coeficiente[[16]])
freelo23$coeficiente[[19]] <- as.numeric(freelo23$coeficiente[[19]])

freelo23 <- unnest(data = freelo23, cols = coeficiente, keep_empty = TRUE)

# Corregir valor hora de freelos
freelo23$valor_hora[[18]] <- NA

freelo23 <- unnest(data = freelo23, cols = valor_hora, keep_empty = TRUE)

# Guardar los datos en un archivo
write_delim(freelo23, file = "data/freelancers_2023.csv", delim = ";")

# Guardar datos originales 
write_delim(kiwi23, file = "data/kiwi_2023.csv", delim = ";")
