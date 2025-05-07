
# Portafolio de Terapia de Reemplazo Renal Continua (TRRC)

Esta aplicación `Shiny` permite registrar parámetros clínicos diarios, calcular indicadores de calidad, y generar automáticamente una nota estructurada para la historia clínica en pacientes en TRRC.

---

## ✅ Características principales

- Registro diario de variables clínicas, bioquímicas y técnicas
- Cálculo automático de:
  - Gap de ultrafiltración (`gap_uf`)
  - Dosis de ultrafiltración (`dosis_uf`)
- Generación de nota clínica estructurada y editable
- Adaptación del plan de tratamiento según modalidad y tipo de anticoagulación
- Conexión directa a base de datos Supabase
- Validaciones robustas para evitar errores al guardar

---

## 📦 Estructura de la app

- `plan_preservacion(input, dosis_uf)`: Función que adapta el plan terapéutico al tipo de preservación (citrato vs. heparina/lavados)
- `observeEvent(input$guardar, {...})`: Contiene la lógica para:
  - Validar inputs esenciales
  - Calcular dosis y gap
  - Construir la nota clínica con `glue_collapse()`
  - Enviar datos a Supabase como JSON

---

## 🛠️ Validaciones implementadas

- `req(input$id_paciente, input$fecha)`: previene registros incompletos
- `tryCatch(...)`: protege cálculos contra divisiones inválidas
- `lapply(..., if NA then NULL)`: asegura compatibilidad JSON con Supabase

---

## 🚀 Despliegue

1. Crear base de datos en Supabase con columnas listadas en `datos <- list(...)`
2. Configurar claves de API y URL
3. Desplegar en shinyapps.io o correr localmente con `shiny::runApp("app.R")`

---

## ✍️ Autor

Desarrollado por **Juan Castellanos de la Hoz**, nefrólogo y educador clínico en Universidad del Rosario y Fundación Cardioinfantil, como parte de su proyecto académico en el MSc Clinical Education (University of Edinburgh).

---
