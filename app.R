library(shiny)
library(httr)
library(jsonlite)
library(stringr)
library(glue)
# en la parte superior de app.R
library(shinyjs)

# Función condicional para el plan según estrategia de preservación
plan_preservacion <- function(input, dosis_uf) {
  if (input$preservacion == "citrato regional") {
    glue_collapse(c(
      "Citrato en PBP; dosis titulable de acuerdo a calcio posfiltro",
      if (!is.na(input$pos_prismocal)) glue("Posdilución {input$pos_prismocal}% con Prism0cal") else NULL,
      if (!is.na(input$hd_prismocal))   glue("Hemodiálisis {input$hd_prismocal}% con Prism0cal") else NULL,
      glue("Extracción de líquido para balance {input$extraccion} mL en 24 horas"),
      glue("Dosis de ultrafiltración {round(dosis_uf, 2)} mL/kg/h"),
      glue("Dosis de efluente {input$dosis_efluente} mL/kg/h"),
      "Qb 110 mL/min",
      "Control de gases venosos cada 4 h para control de calcio iónico pre y posfiltro.",
      "Antagonismo de citrato con gluconato de calcio en jeringa 100% de compensación (dosis de inicio, posterior titulación)",
      "Se formula 50 mg de gluconato de Ca para circuito"
    ), sep = "\n")
  } else {
    glue_collapse(c(
      if (!is.na(input$pre_prismasate)) glue("Predilución {input$pre_prismasate}% con Prismasate {input$tipo_pre}") else NULL,
      if (!is.na(input$pos_prismasate)) glue("Posdilución {input$pos_prismasate}% con Prismasate {input$tipo_pos}") else NULL,
      if (!is.na(input$hd_prismasate))  glue("Hemodiálisis {input$hd_prismasate}% con Prismasate {input$tipo_hd}") else NULL,
      glue("Extracción de líquido para balance {input$extraccion} mL en 24 horas"),
      glue("Dosis de ultrafiltración {round(dosis_uf, 2)} mL/kg/h"),
      if (!is.na(input$dosis_efluente)) glue("Dosis de efluente {input$dosis_efluente} mL/kg/h") else NULL,
      glue("Qb {input$qb} mL/min")
    ), sep = "\n")
  }
}

# Cargar variables de entorno para mayor seguridad
# source("keys.R")

source("keys.R")

supabase_url <- supabase_url_secret
supabase_key <- supabase_key_secret
tabla <- "datos_trrc"

ui <- fluidPage(
  titlePanel("TRRC - Registro + Nota Clínica"),
  useShinyjs()  ,  # Habilitar shinyjs
  sidebarLayout(
    sidebarPanel(
      # Datos del paciente
      h2("🛌 Datos del paciente"),
      textInput("id_paciente", "ID del paciente"),
      dateInput("fecha", "Fecha del registro", value = Sys.Date()),
      numericInput("peso", "Peso (kg)", value = 70),
      numericInput("horas_filtro", "Horas de uso del filtro", value = NA),
      numericInput("administrados", "Líquidos administrados (ml/24h)", value = NA),
      numericInput("eliminados", "Líquidos eliminados (ml/24h)", value = NA),
      numericInput("diuresis", "Diuresis (ml/d)", value = NA),
      #numericInput("uf_24h", "UF neta (ml/24h)", value = NA),
      numericInput("uf_meta", "Meta previa de UFnet (ml)", value = NA),
      numericInput("dosis_prescrita", "Dosis prescrita (ml/kg/h)", value = NA),
      numericInput("dosis_entregada", "Dosis entregada (ml/kg/h)", value = NA),
      # Laboratorios
      h2("🔬 Laboratorio"),
      numericInput("na", "Na (mmol/L)", value = NA),
      numericInput("k", "K (mmol/L)", value = NA),
      numericInput("ca", "Ca (mg/dL)", value = NA),
      numericInput("cai", "Ca ionico (mmol/L)", value = NA),
      numericInput("mg", "Mg (mg/dL)", value = NA),
      numericInput("cl", "Cl (mmol/L)", value = NA),
      numericInput("fosforo", "PO4 (mg/dL)", value = NA),
      numericInput("hcto", "Hematocrito (%)", value = NA),
      numericInput("nus", "NUS (mg/dL)", value = NA),
      numericInput("cr", "Creatinina (mg/dL)", value = NA),
      numericInput("hco3", "HCO3 (mmol/L)", value = NA),
      numericInput("lactato", "Lactato (mmol/L)", value = NA),
      # Soportes
      h2("🆘 Soportes"),
      checkboxGroupInput("soporteVP", "Soporte vasopresor",
                         choices = c(
                           "Norepinefrina" = "norepi",
                           "Vasopresina"      = "vaso",
                           "Dobutamina"       = "db",
                           "Levosimendán"     = "lvmd",
                           "Balón contrapulsación" = "bcp",
                           "Sin soporte" = "no"), 
                         selected = c("no")),
      selectInput("soporteV", "Soporte ventilatorio",
                  choices = c("VMI","VMNI","Máscara alto flujo","Cánula nasal","Sin soporte")),
      # Prescripción TRRC
      h2("℞ Prescripción TRRC"),
      selectInput("modalidad", "Modalidad TRRC",
                  choices = c("🔵🔴 HDFVVC","🔴 HFVVC","🔵 HDVVC","🟢 SCUF")),
      numericInput("dosis_efluente", "Dosis de efluente (ml/kg/h)", value = 25),
      selectInput("preservacion", "Preservación",
                  choices = c("citrato regional","lavados","heparina")),
      numericInput("extraccion", "Extracción para balance (ml/24h)", value = -1000),
      wellPanel(
        h4("💦 Ultrafiltracion neta (UF net)"),
        textOutput("dosis_uf_auto")
      ),
      # numericInput("dosis_efluente", "Dosis de efluente (ml/kg/h)", value = NA),
      # numericInput("dosis_uf", "Dosis de UF (ml/kg/h)", value = NA),  # bamos a generar la dosis de UF automaticamente
      
      # Lavados con SSN / Heparina
      conditionalPanel(
        h3("🌊 Lavados con SSN / 🩸🟣 Heparina"),
        condition = "input.preservacion == 'lavados'| input.preservacion == 'heparina'",
      numericInput("qb", "Qb (ml/min)", value = 180),
      conditionalPanel(
        condition = "input.modalidad == '🔵🔴 HDFVVC'",
        numericInput("pre_prismasate", "🔴 Predilución Prismasate (%)", value = NA),
        selectInput("tipo_pre", "Tipo predilución", choices = c("4/2.5","2/0")),
        numericInput("pos_prismasate", "🔴 Posdilución Prismasate (%)", value = NA),
        selectInput("tipo_pos", "Tipo posdilución", choices = c("4/2.5","2/0")),
        numericInput("hd_prismasate", "🔵 HD Prismasate (%)", value = NA),
        selectInput("tipo_hd", "Tipo HD", choices = c("4/2.5","2/0"))
      ),
      conditionalPanel(
        condition = "input.modalidad == '🔴 HFVVC'",
        numericInput("pre_prismasate", "🔴 Predilución Prismasate (%)", value = NA),
        selectInput("tipo_pre", "Tipo predilución", choices = c("4/2.5","2/0")),
        numericInput("pos_prismasate", "🔴 Posdilución Prismasate (%)", value = NA),
        selectInput("tipo_pos", "Tipo posdilución", choices = c("4/2.5","2/0"))
      ),
      conditionalPanel(
        condition = "input.modalidad == '🔵 HDVVC'",
        numericInput("hd_prismasate", "🔵 HD Prismasate (%)", value = NA),
        selectInput("tipo_hd", "Tipo HD", choices = c("4/2.5","2/0"))
        ),
      ),
      
      # Citrato
      conditionalPanel(
        h3("🍋🧪 Citrato"),
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
      # en UI
      actionButton("copy_btn", "📋 Copiar nota"),
      tags$script(HTML("
  Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
    var ta = document.createElement('textarea');
    ta.value = text;
    document.body.appendChild(ta);
    ta.select();
    document.execCommand('copy');
    document.body.removeChild(ta);
  });
")),
      verbatimTextOutput("texto_final"),
      verbatimTextOutput("respuesta_supabase")
      )
    )
  )





server <- function(input, output, session) {
  
  dataInput <- reactive({
    # Validar que las entradas no sean NA o negativas
    if (is.na(input$extraccion) || is.na(input$peso)) {
      return(list(error = "⚠️ Complete todos los campos requeridos: extracción y peso."))
    }
    if (input$peso <= 0) {
      return(list(error = "⚠️ El peso debe ser mayor que cero."))
    }
    # if (input$extraccion <= 0) {
    #   return(list(error = "⚠️ La extracción debe ser mayor que cero."))
    # }
    
    list(
      extraccion = as.numeric(input$extraccion),
      peso = as.numeric(input$peso),
      error = NULL
    )
  })
  
  
  output$dosis_uf_auto <- renderText({
    datos <- dataInput()
    
    # Mostrar errores si existen
    if (!is.null(datos$error)) {
      return(datos$error)
    }
    
    # Calcular la dosis de ultrafiltración
    dosis_uf <- (datos$extraccion / datos$peso) / 24
    paste("Dosis de UF calculada:", round(dosis_uf, 2), "mL/kg/h")
  })
  
  
  observeEvent(input$guardar, {
    # Validación mínima (descomentar si se desea)
    # validate(
    #   need(input$id_paciente != "", "Falta el ID del paciente"),
    #   need(!is.na(input$fecha), "Falta la fecha"),
    #   need(!is.na(input$dosis_prescrita), "Falta dosis prescrita"),
    #   need(input$modalidad != "", "Falta modalidad de TRRC")
    # )
    
    req(input$id_paciente, input$fecha)
    
    balance <- tryCatch(input$administrados - input$eliminados, error = function(e) NULL)
    gap_uf  <- tryCatch(round((input$uf_meta - balance) / input$uf_meta * 100, 1), error = function(e) NULL)
    dosis_uf <- tryCatch((input$extraccion / input$peso) / 24, error = function(e) NULL)

    
    
    # Generación de la nota clínica
    nota <- glue_collapse(c(
      "Paciente con terapia de reemplazo renal continuo como soporte vital.",
      if (!is.na(input$horas_filtro))    glue("Su filtro actual tiene {input$horas_filtro} horas de funcionamiento.") else NULL,
      if (!is.na(balance))               glue("UFnet: {balance} mL/24h")                     else NULL,
      if (!is.na(input$uf_meta))         glue("Meta previa de UFnet : {input$uf_meta} mL/24h") else NULL,
      if (!is.na(gap_uf))                glue("Gap UFnet: {gap_uf} %")                     else NULL,
      if (!is.na(input$diuresis))        glue("Diuresis: {input$diuresis} mL/d")                    else NULL,
      if (!is.na(input$dosis_prescrita)) glue("Dosis prescrita: {input$dosis_prescrita} mL/kg/h") else NULL,
      if (!is.na(input$dosis_entregada)) glue("Dosis entregada: {input$dosis_entregada} mL/kg/h") else NULL,
      if (!is.na(input$soporteVP))       glue("Soporte vasopresor: {paste(input$soporteVP, collapse = ', ')}") else NULL,
      if (!is.na(input$soporteV))        glue("Soporte ventilatorio: {input$soporteV}") else NULL,
      "Perfil laboratorios:",
      glue("PO4 {input$fosforo} mg/dL | Mg {input$mg} mg/dL | K {input$k} mmol/L | Na {input$na} mmol/L | Cl {input$cl} mmol/L"),
      glue("Ca {input$ca} mg/dL | Cai {input$cai} mmol/L | Lactato {input$lactato} mmol/L | HCO3 {input$hco3} mmol/L"),
      glue("NUS {input$nus} mg/dL | Creatinina {input$cr} mg/dL | Hcto {input$hcto}%"),
      "PLAN:",
      glue("{input$modalidad} con preservación: {input$preservacion}"),
      plan_preservacion(input, dosis_uf)
    ), sep = "\n")
    
    output$texto_final <- renderText(nota)
    
    ##### boton para copiar la nota ####
    # después de generar 'nota' en tu observeEvent(input$guardar,...)
    # crea un reactiveVal para guardar la nota
    
    nota_val <- reactiveVal("")
    
    observe({
      # supongamos que 'nota' es un reactive o una variable local
      # si la construyes en un observeEvent, guárdala en un reactiveVal():
      nota_val(nota)  # si usas: nota_val <- reactiveVal()
      updateTextAreaInput(session, "nota_area", value = nota)
    })
    

    #####

    
    # Preparar datos para Supabase
    datos <- list(
      id_paciente                  = input$id_paciente,
      fecha                        = as.character(input$fecha),
      peso_kg                      = input$peso,
      horas_funcionamiento_filtro  = input$horas_filtro,
      diuresis_ml_dia              = as.integer(input$diuresis),
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
      calcio_total                 = as.numeric(input$ca),
      calcio_ionico                = as.numeric(input$cai),
      lactato_mmol_l               = input$lactato,
      hco3_mmol_l                  = input$hco3,
      nus_mg_dl                    = input$nus,
      cr_mg_dl                     = input$cr,
      hcto_percent                 = input$hcto,
      modalidad_trrc               = input$modalidad,
      estrategia_anticoagulacion   = input$preservacion,
      pos_prismocal_pct            = input$pos_prismocal,
      hd_prismocal_pct             = input$hd_prismocal,
      extraccion_liquido_24h_ml    = as.integer(input$extraccion),
      dosis_uf_ml_kg_h             = input$dosis_uf,
      qb_ml_min                    = input$qb,
      pre_prismasate_pct           = input$pre_prismasate,
      tipo_pre_prismasate          = input$tipo_pre,
      pos_prismasate_pct           = input$pos_prismasate,
      tipo_pos_prismasate          = input$tipo_pos,
      hd_prismasate_pct            = input$hd_prismasate,
      tipo_hd_prismasate           = input$tipo_hd,
      realizada_por_residente      = input$residente,
      id_residente                 =  if (!is.null(input$residente) && isTRUE(input$residente)) input$id_residente else NULL,
      observaciones                = input$observaciones,
      soporte_vp                   = paste(input$soporteVP, collapse = ", "),
      soporte_v                    = input$soporteV,
      administrados                = input$administrados,
      eliminados                   = input$eliminados,
      gap_uf                       = gap_uf,
      nota_clinica_generada        = nota
    )
    
    # 2. Reemplaza NAs y vectores vacíos por NULL
    datos_nulls <- lapply(datos, function(x) {
      if (is.null(x) || length(x) == 0 || all(is.na(x))) {
        NULL
      } else {
        x
      }
    })
    
    # datos <- list(
    #   k_mmol_l = ifelse(is.na(input$k), NULL, as.numeric(input$k)),
    #   na_mmol_l = ifelse(is.na(input$na), NULL, as.numeric(input$na))
    # )
   
    # Convertir NA a NULL


    # cuerpo <- jsonlite::toJSON(datos, auto_unbox = TRUE, null = "null")
    # cat("JSON enviado:", cuerpo, "\n")
    # 
    # 1. Filtra NULLs de la lista 'datos'
    # 3. Elimina por completo las entradas NULL
    datos_clean <- Filter(Negate(is.null), datos_nulls)
    
    # 4. Manda el POST con encode = "json"
    res <- httr::POST(
      url    = paste0(supabase_url, "/rest/v1/", tabla),
      httr::add_headers(
        apikey        = supabase_key,
        Authorization = paste("Bearer", supabase_key),
        `Content-Type`= "application/json",
        Prefer        = "return=representation"
      ),
      body   = datos_clean,
      encode = "json"
    )
    
    # 5. Debug: imprime el JSON final para verificar que no haya {} ni arrays
    cat("JSON final a enviar:\n", jsonlite::toJSON(datos_clean, auto_unbox=TRUE, na="null"), "\n")
    
    # 6. Manejo de respuesta
    if (httr::status_code(res) %in% c(200, 201)) {
      output$respuesta_supabase <- renderText("✅ Guardado exitoso")
    } else {
      output$respuesta_supabase <- renderText(paste("⚠️ Error:", content(res, "text")))
    }
    
  })
  
  texto_clinico <- reactive({
    verbatimTextOutput("texto_final")
  })
    
  # Copiar nota al portapapeles
  # cuando pulsen 'copy_btn', ejecuta JS para copiar
  observeEvent(input$copy_btn, {
    nota <- input$nota_area
    session$sendCustomMessage("copyToClipboard", nota)
    showNotification("🗒️ Nota copiada al portapapeles", type = "message")
  })


}

shinyApp(ui, server)