# Portafolio TRRC (Terapia de Reemplazo Renal Continua)

Este proyecto es una aplicación clínica desarrollada en **R + Shiny**, diseñada para:

✅ Registrar diariamente parámetros clínicos y terapéuticos de pacientes en TRRC  
✅ Generar automáticamente una **nota clínica estructurada** para la historia clínica  
✅ Almacenar los datos en **Supabase** para su posterior análisis  
✅ Servir como **portafolio de actividades** para residentes de nefrología en formación

---

## ✨ Funcionalidades principales

- Formulario Shiny interactivo con validaciones básicas
- Cálculo automático de:
  - Gap de ultrafiltración (`gap_uf`)
  - Índice de calcio (cuando se usa citrato)
- Generación de plan terapéutico adaptado a la estrategia de preservación del filtro
- Almacenamiento en Supabase con trazabilidad por residente
- Lógica de conexión robusta con validación de errores en Supabase

---

## 💻 Requisitos

- R (≥ 4.0)
- Paquetes:
  - `shiny`
  - `httr`
  - `jsonlite`
  - `stringr`
  - `glue`
  
---

## 🧪 Estado actual

- [x] Registro diario funcional
- [x] Integración con Supabase
- [x] Validación robusta de respuestas
- [ ] Vista de datos por paciente/residente (en desarrollo)
- [ ] Exportación en PDF o impresión de notas (planeado)

---

## 📚 Autores y créditos

Proyecto desarrollado por **Juan Castellanos de la Hoz**, nefrólogo clínico y educador en la Universidad del Rosario y Fundación Cardioinfantil (Bogotá, Colombia).  
Desarrollado como parte de su portafolio de evaluación para el MSc Clinical Education (University of Edinburgh).

---