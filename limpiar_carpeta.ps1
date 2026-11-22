# Limpieza de la carpeta de la tesis
# Mueve a _to_delete\ los archivos que el libro no usa (diagramas, restos de
# plantilla/renders y la carpeta poster\). Revisa _to_delete\ y luego bórrala.
# Ejecutar desde la carpeta de la tesis:
#   powershell -ExecutionPolicy Bypass -File .\limpiar_carpeta.ps1

$ErrorActionPreference = "Stop"
Set-Location -LiteralPath $PSScriptRoot
$destino = "_to_delete"
New-Item -ItemType Directory -Force -Path $destino | Out-Null

$items = @(
  # Diagramas
  "esquema_hipotesis.dot", "esquema_hipotesis.png", "esquema_hipotesis.py", "esquema_hipotesis.svg",
  "modelo_conceptual.png", "modelo_conceptual.py", "modelo_conceptual.svg",
  # Restos de plantilla y renders
  "README.html", "search.json", "reportes-facso-plantilla.tex", "testnofig.tex",
  "04-resultados_files", "library(tidyverse).r",
  "procesamiento\exploracion.r",
  "refs\referencias.bib",
  "input\data",
  "assets\favicon.svg", "assets\file.svg", "assets\sharing-default.png",
  "includes\title-html.html", "includes\pdf-config-scrbook.yml", "includes\reference.docx",
  "includes\reportes-facso-plantilla.tex",
  # Póster (queda la versión de EDUMER-DSE)
  "poster"
)

foreach ($i in $items) {
  if (Test-Path -LiteralPath $i) {
    $dest = Join-Path $destino $i
    New-Item -ItemType Directory -Force -Path (Split-Path -Parent $dest) | Out-Null
    Move-Item -LiteralPath $i -Destination $dest
    Write-Host "Movido: $i"
  } else {
    Write-Host "No existe (se omite): $i"
  }
}
Write-Host ""
Write-Host "Listo. Revisa la carpeta _to_delete y bórrala antes de hacer commit."
