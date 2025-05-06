library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(glue)

# Función condicional para el plan según estrategia de preservación
plan_preservacion <- function(input) {
  if (input$preservacion == "citrato regional") {
    glue_collapse(c(
      "Citrato en PBP; dosis titulable de acuerdo a calcio posfiltro",
      if (!is.na(input$pos_prismocal)) glue("Posdilución {input$pos_prismocal}% con Prism0cal") else NULL,
      if (!is.na(input$hd_prismocal))   glue("Hemodiálisis {input$hd_prismocal}% con Prism0cal") else NULL,
      glue("Extracción de líquido para balance {input$extraccion} en 24 horas"),
      # glue("Dosis de ultrafiltración {input$dosis_uf} mL/kg/h"),
      "Dosis de efluente 110 ml/Kg/min",
      glue("Dosis de efluente {input$dosis_efluente} mL/kg/h"),
      glue("Qb {input$qb} mL/min"),
      "Control de gases venosos cada 4 h para control de calcio iónico pre y posfiltro.",
      "Antagonismo de citrato con gluconato de calcio en jeringa 100% de compensación (dosis de inicio, posterior titulación)",
      "Se formula 50 mg de gluconato de Ca para circuito"
    ), sep = "\n")
  } else {
    glue_collapse(c(
      if (!is.na(input$pre_prismasate)) glue("Predilución {input$pre_prismasate}% con Prismasate {input$tipo_pre}") else NULL,
      if (!is.na(input$pos_prismasate)) glue("Posdilución {input$pos_prismasate}% con Prismasate {input$tipo_pos}") else NULL,
      if (!is.na(input$hd_prismasate))  glue("Hemodiálisis {input$hd_prismasate}% con Prismasate {input$tipo_hd}") else NULL,
      glue("Extracción de líquido para balance {input$extraccion} en 24 horas"),
      glue("Dosis de ultrafiltración {input$dosis_uf} mL/kg/h"),
      glue("Qb {input$qb} mL/min")
    ), sep = "\n")
  }
}

# Cargar variables de entorno para mayor seguridad
supabase_url <- "https://cwpcpdgjncvtghmgwvix.supabase.co"
supabase_key <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImN3cGNwZGdqbmN2dGdobWd3dml4Iiwicm9sZSI6ImFub24iLCJpYXQiOjE3NDMyODAyMTksImV4cCI6MjA1ODg1NjIxOX0.Vikhe8J17HdSlJQ3e5VdgTsAsrSEHOhTFfhrmqjKt1w"
tabla <- "datos_trrc"

ui <- fluidPage(
  titlePanel("TRRC - Registro + Nota Clínica"),
  sidebarLayout(
    sidebarPanel(
      # Datos del paciente
      h2("Datos del paciente"),
      textInput("id_paciente", "ID del paciente"),
      dateInput("fecha", "Fecha del registro", value = Sys.Date()),
      numericInput("peso", "Peso (kg)", value = NA),
      numericInput("horas_filtro", "Horas de uso del filtro", value = NA),
      numericInput("administrados", "Líquidos administrados (ml/24h)", value = NA),
      numericInput("eliminados", "Líquidos eliminados (ml/24h)", value = NA),
      numericInput("diuresis", "Diuresis (ml/d)", value = NA),
      numericInput("uf_24h", "UF lograda (ml/24h)", value = NA),
      numericInput("uf_meta", "Meta de UF (ml)", value = NA),
      numericInput("dosis_prescrita", "Dosis prescrita (ml/kg/h)", value = NA),
      numericInput("dosis_entregada", "Dosis entregada (ml/kg/h)", value = NA),
      # Laboratorios
      h2("Laboratorio"),
      numericInput("na", "Na (mmol/L)", value = NA),
      numericInput("k", "K (mmol/L)", value = NA),
      numericInput("cl", "Cl (mmol/L)", value = NA),
      numericInput("fosforo", "PO4 (mg/dL)", value = NA),
      numericInput("mg", "Mg (mg/dL)", value = NA),
      numericInput("hcto", "Hematocrito (%)", value = NA),
      numericInput("nus", "NUS (mg/dL)", value = NA),
      numericInput("cr", "Creatinina (mg/dL)", value = NA),
      numericInput("ca", "Ca (mg/dL)", value = NA),
      numericInput("cai", "Ca ionico (mmol/L)", value = NA),
      numericInput("hco3", "HCO3 (mmol/L)", value = NA),
      numericInput("lactato", "Lactato (mmol/L)", value = NA),
      # Soportes
      h2("Soportes"),
      checkboxGroupInput("soporteVP", "Soporte vasopresor",
                         choices = c(
                           "Norepinefrina" = "norepi",
                           "Vasopresina"      = "vaso",
                           "Dobutamina"       = "db",
                           "Levosimendán"     = "lvmd",
                           "Balón contrapulsación" = "bcp"
                         )),
      selectInput("soporteV", "Soporte ventilatorio",
                  choices = c("VMI","VMNI","Máscara alto flujo","Cánula nasal","Sin soporte")),
      # Prescripción TRRC
      h2("Prescripción TRRC"),
      selectInput("modalidad", "Modalidad TRRC",
                  choices = c("HDFVVC","HFVVC","HDVVC","SCUF")),
      selectInput("preservacion", "Preservación",
                  choices = c("citrato regional","lavados","heparina")),
      numericInput("extraccion", "Extracción para balance (ml/24h)", value = NA),
      numericInput("dosis_efluente", "Dosis de efluente (ml/kg/h)", value = NA),
      numericInput("dosis_uf", "Dosis de UF (ml/kg/h)", value = NA),
      numericInput("qb", "Qb (ml/min)", value = 160),
      numericInput("pre_prismasate", "Predilución Prismasate (%)", value = NA),
      selectInput("tipo_pre", "Tipo predilución", choices = c("4/2.5","2/0")),
      numericInput("pos_prismasate", "Posdilución Prismasate (%)", value = NA),
      selectInput("tipo_pos", "Tipo posdilución", choices = c("4/2.5","2/0")),
      numericInput("hd_prismasate", "HD Prismasate (%)", value = NA),
      selectInput("tipo_hd", "Tipo HD", choices = c("4/2.5","2/0")),
      conditionalPanel(
        condition = "input.preservacion == 'citrato regional'",
        numericInput("pos_prismocal", "Posdilución Prism0cal (%)", value = NA),
        numericInput("hd_prismocal", "Hemodiálisis Prism0cal (%)", value = NA)
      ),
      checkboxInput("residente", "¿Prescripción por residente?", FALSE),
      conditionalPanel(
        condition = "input.residente == true",
        textInput("id_residente", "Correo del residente")
      ),
      textAreaInput("observaciones", "Observaciones clínicas", ""),
      actionButton("guardar", "Guardar todo en Supabase")
    ),
    mainPanel(
      verbatimTextOutput("texto_final"),
      verbatimTextOutput("respuesta_supabase")
    )
  )
)




server <- function(input, output, session) {
  observeEvent(input$guardar, {
    # Validación mínima (descomentar si se desea)
    # validate(
    #   need(input$id_paciente != "", "Falta el ID del paciente"),
    #   need(!is.na(input$fecha), "Falta la fecha"),
    #   need(!is.na(input$dosis_prescrita), "Falta dosis prescrita"),
    #   need(input$modalidad != "", "Falta modalidad de TRRC")
    # )
    
    req(input$id_paciente, input$fecha)
    
    balance <- tryCatch(input$administrados - input$eliminados, error = function(e) NA)
    gap_uf  <- tryCatch(round((input$uf_meta - input$uf_24h) / input$uf_meta * 100, 1), error = function(e) NA)
    
    # Generación de la nota clínica
    nota <- glue_collapse(c(
      "Paciente con terapia de reemplazo renal continuo como soporte vital.",
      if (!is.na(input$horas_filtro)) glue("Su filtro actual tiene {input$horas_filtro} horas de funcionamiento.") else NULL,
      if (!is.na(balance))           glue("Balance hídrico: {balance} mL/24h")                     else NULL,
      if (!is.na(input$diuresis))    glue("Diuresis: {input$diuresis} mL/d")                    else NULL,
      if (!is.na(input$dosis_prescrita)) glue("Dosis prescrita: {input$dosis_prescrita} mL/kg/h") else NULL,
      if (!is.na(input$dosis_entregada)) glue("Dosis entregada: {input$dosis_entregada} mL/kg/h") else NULL,
      if (!is.na(gap_uf))            glue("Gap ultrafiltración: {gap_uf} %")                     else NULL,
      "Perfil laboratorios:",
      glue("PO4 {input$fosforo} mg/dL | Mg {input$mg} mg/dL | K {input$k} mmol/L | Na {input$na} mmol/L | Cl {input$cl} mmol/L"),
      glue("Ca {input$ca} mg/dL | Cai {input$cai} mmol/L | Lactato {input$lactato} mmol/L | HCO3 {input$hco3} mmol/L"),
      glue("NUS {input$nus} mg/dL | Creatinina {input$cr} mg/dL | Hcto {input$hcto}%"),
      "PLAN:",
      glue("{input$modalidad} con preservación: {input$preservacion}"),
      plan_preservacion(input)
    ), sep = "\n")
    
    output$texto_final <- renderText(nota)
    
    # Preparar datos para Supabase
    datos <- list(
      id_paciente                  = input$id_paciente,
      fecha                        = as.character(input$fecha),
      peso_kg                      = input$peso,
      horas_funcionamiento_filtro  = input$horas_filtro,
      diuresis_ml_dia              = input$diuresis,
      balance_hidrico_ml           = balance,
      uf_24h_ml                    = input$uf_24h,
      uf_meta_prev_ml              = input$uf_meta,
      dosis_prescrita_ml_kg_h      = input$dosis_prescrita,
      dosis_efluente_entregada_ml_kg_h = input$dosis_entregada,
      po4_mg_dl                    = input$fosforo,
      mg_mg_dl                     = input$mg,
      k_mmol_l                     = input$k,
      na_mmol_l                    = input$na,
      cl_mmol_l                    = input$cl,
      calcio_total                 = input$ca,
      calcio_ionico                = input$cai,
      lactato_mmol_l               = input$lactato,
      hco3_mmol_l                  = input$hco3,
      nus_mg_dl                    = input$nus,
      cr_mg_dl                     = input$cr,
      hcto_percent                 = input$hcto,
      modalidad_trrc               = input$modalidad,
      estrategia_anticoagulacion   = input$preservacion,
      pos_prismocal_pct            = input$pos_prismocal,
      hd_prismocal_pct             = input$hd_prismocal,
      extraccion_liquido_24h_ml    = input$extraccion,
      dosis_uf_ml_kg_h             = input$dosis_uf,
      qb_ml_min                    = input$qb,
      pre_prismasate_pct           = input$pre_prismasate,
      tipo_pre_prismasate          = input$tipo_pre,
      pos_prismasate_pct           = input$pos_prismasate,
      tipo_pos_prismasate          = input$tipo_pos,
      hd_prismasate_pct            = input$hd_prismasate,
      tipo_hd_prismasate           = input$tipo_hd,
      realizada_por_residente      = input$residente,
      id_residente                 = if (input$residente) input$id_residente else NA,
      observaciones                = input$observaciones,
      soporte_VP                   = paste(input$soporteVP, collapse = ", "),
      soporte_V                    = input$soporteV,
      administrados                = input$administrados,
      eliminados                   = input$eliminados,
      gap_uf                       = gap_uf,
      nota_clinica_generada        = nota
    )
    
    # Envío a Supabase
    res <- POST(
      url    = paste0(supabase_url, "/rest/v1/", tabla),
      add_headers(
        apikey        = supabase_key,
        Authorization = paste("Bearer", supabase_key),
        `Content-Type`= "application/json",
        Prefer        = "return=representation"
      ),
      body = toJSON(datos, auto_unbox = TRUE)
    )
    
    resultado <- content(res, as = "parsed", simplifyVector = TRUE)
    mensaje <- if (!is.null(resultado$id_paciente)) {
      paste0("✅ Registro guardado exitosamente para el paciente ",
             resultado$id_paciente,
             " el día ", resultado$fecha, ".")
    } else {
      "⚠️ Error al guardar. Revisa los datos o conexión a Supabase."
    }
    output$respuesta_supabase <- renderText(mensaje)
    
  })
}

shinyApp(ui, server)