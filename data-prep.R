library(perutranspaeconomica)
library(perumapas)
library(tidyverse)
library(sf)
library(flextable)

vraem <- readxl::read_excel("data/distritos_vraem.xlsx") |> 
    mutate(across(.fns = casemisc::str_limpiar))

translate_pliego <- function(pliego) {
    case_when(
        pliego == "444" ~ "AYACUCHO",
        pliego == "446" ~ "CUSCO",
        pliego == "450" ~ "JUNIN",
        TRUE ~ pliego
    )
}

doc_tbl <- function(df) {
    df |> 
        rename_with(~str_replace_all(.x, "_", " ")) |> 
        flextable() |> 
        theme_box() |> 
        align(part = 'header', align = 'center') |> 
        fontsize(size = 9, part = 'body') |> 
        fontsize(size = 10, part = 'header') |> 
        font(fontname = 'Calibri', part = 'all') |> 
        set_table_properties("autofit")
}

get_vraem_distritos <- function(.departamento) {
    vraem |> 
        filter(departamento == .departamento) |> 
        doc_tbl()
}

vraem_distritos <- list(
    "JUNIN" = get_vraem_distritos("JUNIN"),
    "CUSCO" = get_vraem_distritos("CUSCO"),
    "AYACUCHO" = get_vraem_distritos("AYACUCHO")
)

distritos_nuevos <- vraem |> 
    anti_join(mapa_distrital) |> 
    doc_tbl()

distritos_antiguos <- vraem |> 
    semi_join(mapa_distrital) |>
    left_join(mapa_distrital)

provincias <- distritos_antiguos |> 
    distinct(departamento, provincia) |> 
    left_join(mapa_provincial)

regiones <- provincias |> 
    distinct(departamento) |> 
    left_join(mapa_regional)

vraem_mapa <- ggplot() +
    geom_sf(data = provincias, aes(geometry = geometry, fill = departamento), color = "grey30") +
    geom_sf(data = distritos_antiguos, aes(geometry = geometry), fill = "black", alpha = 0.5, color = "grey50") +
    geom_sf_label(data = provincias, aes(geometry = geometry, label = provincia), size = 3, alpha = 0.4) +
    scale_fill_manual(values = c("cadetblue3", "aquamarine3", "deepskyblue3"), guide = guide_legend(override.aes = list(size = NULL))) +
    theme_void() +
    theme(legend.position = "top", 
          legend.title = element_blank()) +
    labs(
        caption = "Las etiquetas corresponden al nombre de provincia\nLa zona sombreada corresponde a los distritos dentro de la zona VRAEM"
    ) 

presupuesto_regiones <- seguimiento_ep() |> 
    elegir_periodo_anual(periodo = 2022) |> 
    elegir_quien_gasta(
        nivel = "R",
        sector = "99", 
        pliego = "todos"
    ) |> 
    consultar() 

presupuesto_regiones_vraem <- presupuesto_regiones |> 
    mutate(desc_pliego = str_remove(desc_pliego, "GOBIERNO REGIONAL DEL DEPARTAMENTO DE ")) |> 
    rename(desc_departamento = desc_pliego) |> 
    semi_join(vraem, by = c("desc_departamento" = "departamento")) |> 
    left_join(regiones, by = c("desc_departamento" = "departamento")) |> 
    select(-geometry) |> 
    rename(cod_departamento = cod_depa)

presupuesto_provincias <- seguimiento_ep() |> 
    elegir_periodo_anual(periodo = 2022) |> 
    elegir_quien_gasta(
        nivel = "M", 
        goblocal_o_manc = "M",
        departamento = presupuesto_regiones_vraem$cod_departamento, 
        provincia = "todos"
    ) |> 
    consultar() 

presupuesto_provincias_vraem <- presupuesto_provincias |> 
    semi_join(vraem, by = c("desc_provincia" = "provincia"))

presupuesto_distritos <- seguimiento_ep() |> 
    elegir_periodo_anual(periodo = 2022) |> 
    elegir_quien_gasta(
        nivel = "M", 
        goblocal_o_manc = "M",
        departamento = presupuesto_regiones_vraem$cod_departamento, 
        municipalidad = "todos"
    ) |> 
    consultar() |> 
    filter(str_detect(desc_municipalidad, "MUNICIPALIDAD DISTRITAL")) |> 
    rename(cod_departamento = departamento) |> 
    mutate(
        desc_municipalidad = str_remove(desc_municipalidad, "MUNICIPALIDAD DISTRITAL (DE )?"),
        cod_municipalidad_mef = str_remove(cod_municipalidad, ".*-"),
        cod_municipalidad = str_remove(cod_municipalidad, "-.*"),
        cod_provincia = str_extract(cod_municipalidad, "^....")
    ) |> 
    semi_join(presupuesto_provincias, by = "cod_provincia") |> 
    mutate(desc_municipalidad = case_when(
        desc_municipalidad == "QUIMBIRI" ~ "KIMBIRI",
        TRUE ~ desc_municipalidad
    ))

presupuesto_distritos_vraem <- presupuesto_distritos |> 
    semi_join(vraem, by = c("desc_municipalidad" = "distrito"))


ejecutado_regiones <- presupuesto_regiones_vraem |> 
    select(departamento = desc_departamento, pim, avance_percent)

tabla_presupuesto_regiones <- ejecutado_regiones |> 
    select(-avance_percent) |> 
    arrange(desc(pim)) |> 
    doc_tbl()

ejecutado_provincias <- presupuesto_provincias_vraem |> 
    select(cod_depa = departamento, provincia = desc_provincia, pim, avance_percent) |> 
    left_join(regiones) |> 
    select(departamento, provincia, pim, avance_percent)

tabla_presupuesto_provincias <- ejecutado_provincias |> 
    select(-avance_percent) |> 
    arrange(desc(pim)) |> 
    doc_tbl()

ejecutado_distritos <- presupuesto_distritos_vraem |> 
    select(cod_depa = cod_departamento, cod_provincia, municipio = desc_municipalidad, pim, avance_percent) |> 
    left_join(regiones) |> 
    select(departamento, cod_provincia:avance_percent) |> 
    left_join(provincias, by = c("cod_provincia" = "cod_prov")) |> 
    select(departamento = departamento.x, provincia, municipio:avance_percent)

get_tabla_presupuesto_distritos <- function(.departamento) {
    ejecutado_distritos |> 
        filter(departamento == .departamento) |>  
        select(-avance_percent, -departamento) |> 
        arrange(desc(pim)) |> 
        doc_tbl()
}

tablas_presupuesto_distritos <- list(
    "AYACUCHO" = get_tabla_presupuesto_distritos("AYACUCHO"),
    "CUSCO" = get_tabla_presupuesto_distritos("CUSCO"),
    "JUNIN" = get_tabla_presupuesto_distritos("JUNIN")
)

mapa_ejecutado_regiones <- ejecutado_regiones |> 
    left_join(regiones) |> 
    ggplot(aes(geometry = geometry)) +
    geom_sf(aes(fill = departamento)) +
    scale_fill_manual(values = c("cadetblue3", "aquamarine3", "deepskyblue3"), guide = guide_legend(override.aes = list(size = NULL))) +
    geom_sf_label(aes(label = paste0(departamento, ": ", avance_percent))) +
    theme_void() +
    theme(legend.position = "none")

mapa_ejecutado_provincias <- ejecutado_provincias |> 
    left_join(provincias) |> 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = departamento), data = regiones) +
    geom_sf(aes(geometry = geometry), fill = alpha("black", 0.5), color = "grey50") +
    geom_sf_label(aes(geometry = geometry, label = paste0(provincia, ": ", avance_percent, "%")), size = 3, alpha = 0.4) +
    scale_fill_manual(values = c("cadetblue3", "aquamarine3", "deepskyblue3"), guide = guide_legend(override.aes = list(size = NULL))) +
    theme_void() +
    theme(legend.position = "top", legend.title = element_blank()) +
    labs(
        caption = "La zona sombreada corresponde al territorio total de la provincia\ncon al menos un distrito considerado en la zona VRAEM"
    )

mapa_ejecucion_distrital <- ejecutado_distritos |> 
    left_join(mapa_distrital, by = c("departamento", "provincia", "municipio" = "distrito")) |> 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = departamento), provincias) +
    geom_sf(aes(geometry = geometry), fill = alpha("grey20", 0.5)) +
    ggrepel::geom_label_repel(aes(geometry = geometry, label = paste0(municipio, ": ", avance_percent, "%")), stat = "sf_coordinates", size = 2.5, fill = alpha("white", 0.5)) +
    scale_fill_manual(values = c("cadetblue3", "aquamarine3", "deepskyblue3"), guide = guide_legend(override.aes = list(size = NULL))) +
    theme_void() +
    theme(legend.position = "top", legend.title = element_blank()) +
    labs(
        caption = "Las etiquetas corresponden al nombre del distrito\nLa zona sombreada corresponde alos distritos dentro de la zona VRAEM"
    )

ggsave("mapas/mapa_ejecucion_distrital.png", plot = mapa_ejecucion_distrital)


financiamiento_regiones <- seguimiento_ep() |> 
    elegir_periodo_anual(periodo = 2022) |> 
    elegir_quien_gasta(
        nivel = "R", 
        sector = "99", 
        pliego = c("444", "446", "450")
    ) |> 
    elegir_con_que_se_financia(fuente_financiamiento = 1:5, rubro = "todos") |> 
    consultar()

tabla_financiamiento_regiones <- financiamiento_regiones |> 
    as_tibble() |> 
    mutate(departamento = translate_pliego(pliego)) |> 
    group_by(departamento) |> 
    mutate(porcentaje = (pim/sum(pim)*100) |> round(1)) |> 
    ungroup() |> 
    filter(cod_rubro == 18) |> 
    select(departamento, pim_rubro = pim, peso_rubro = porcentaje, ejecutado_rubro = avance_percent) |> 
    doc_tbl()

financiamiento_provincias <- provincias |> 
    select(cod_prov) |> 
    separate(cod_prov, into = c("departamento", "provincia"), sep = 2) |> 
    mutate(data = map2(departamento, provincia, ~{
        seguimiento_ep() |> 
            elegir_periodo_anual(periodo = 2022) |> 
            elegir_quien_gasta(
                nivel = "M", 
                goblocal_o_manc = "M",
                departamento = .x, 
                provincia = .y
            ) |> 
            elegir_con_que_se_financia(fuente_financiamiento = 1:5, rubro = "todos") |> 
            consultar()
    })) |> 
    select(data) |> 
    unnest(data)

tabla_financiamiento_provincias <- financiamiento_provincias |> 
    as_tibble() |> 
    group_by(departamento, provincia) |> 
    mutate(
        porcentaje = (pim/sum(pim)*100) |> round(1),
        cod_prov = paste0(departamento, provincia)
    ) |> 
    ungroup() |> 
    filter(cod_rubro == 18) |> 
    select(cod_prov, pim, peso = porcentaje, ejecutado_rubro = avance_percent) |> 
    left_join(provincias) |> 
    select(region = departamento, provincia, pim_rubro = pim, pero_rubro = peso, ejecutado_rubro) |> 
    doc_tbl()

financiamiento_distritos <- seguimiento_ep() |> 
    elegir_periodo_anual(periodo = 2022) |> 
    elegir_quien_gasta(
        nivel = "M",
        goblocal_o_manc = "M",
        municipalidad = presupuesto_distritos_vraem$cod_municipalidad_mef
    ) |> 
    elegir_con_que_se_financia(rubro = "todos") |> 
    consultar()

financiamiento_distritos_limpio <- financiamiento_distritos |> 
    group_by(municipalidad) |> 
    mutate(
        peso_rubro = (pim/sum(pim) * 100) |> round(1)
    ) |> 
    ungroup() |> 
    filter(cod_rubro == 18) |> 
    select(cod_municipalidad_mef = municipalidad,
           pim_rubro = pim, peso_rubro, ejecucion_rubro = avance_percent) |> 
    left_join(select(presupuesto_distritos_vraem, cod_municipalidad_mef, desc_municipalidad)) |> 
    # select(-cod_municipalidad_mef) |> 
    rename(distrito = desc_municipalidad) |> 
    left_join(vraem) |> 
    select(departamento, provincia, distrito, pim_rubro, peso_rubro, ejecucion_rubro) |> 
    arrange(desc(pim_rubro))

get_financiamiento_distritos <- function(.departamento) {
    financiamiento_distritos_limpio |> 
        filter(departamento == .departamento) |> 
        select(-departamento) |> 
        doc_tbl()
}

tablas_financiamiento_distritos <- list(
    "AYACUCHO" = get_financiamiento_distritos("AYACUCHO"),
    "CUSCO" = get_financiamiento_distritos("CUSCO"),
    "JUNIN" = get_financiamiento_distritos("JUNIN")
)




## output ----

reporte <- list(
    vraem = vraem,
    vraem_distritos = vraem_distritos,
    vraem_distritos_nuevos = distritos_nuevos,
    vraem_mapa = vraem_mapa,
    tabla_presupuesto_regiones = tabla_presupuesto_regiones,
    tabla_presupuesto_provincias = tabla_presupuesto_provincias,
    tablas_presupuesto_distritos = tablas_presupuesto_distritos,
    mapa_ejecutado_regiones = mapa_ejecutado_regiones,
    mapa_ejecutado_provincias = mapa_ejecutado_provincias,
    tabla_financiamiento_regiones = tabla_financiamiento_regiones,
    tabla_financiamiento_provincias = tabla_financiamiento_provincias,
    tablas_financiamiento_distritos = tablas_financiamiento_distritos
    
)

saveRDS(reporte, file = "data/reporte.rds")

