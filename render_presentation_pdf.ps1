# Run from PowerShell: .\render_presentation_pdf.ps1
# Rebuilds the presentation HTML, then saves a continuous PDF beside the Rmd.
$ErrorActionPreference = 'Stop'
Push-Location $PSScriptRoot
try {
    $rscript = 'C:\Program Files\R\R-4.6.1\bin\Rscript.exe'
    $edge = 'C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe'
    $poppler = (Get-Command pdftoppm -ErrorAction Stop).Source
    $runtimeRoot = $poppler.Substring(0, $poppler.IndexOf('\dependencies\'))
    $python = Join-Path $runtimeRoot 'dependencies/python/python.exe'

    $env:R_LIBS_USER = "$env:LOCALAPPDATA/R/win-library/4.6"
    $env:RSTUDIO_PANDOC = 'C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools'
    & $rscript -e 'rmarkdown::render("nature_presentation.Rmd", output_format="html_document", quiet=TRUE)'
    if ($LASTEXITCODE -ne 0) { throw 'HTML rendering failed.' }

    $work = Join-Path $PSScriptRoot 'tmp/pdfs/render'
    New-Item -ItemType Directory -Path $work -Force | Out-Null
    $sourcePdf = Join-Path $work 'pages.pdf'
    if (Test-Path -LiteralPath $sourcePdf) { Remove-Item -LiteralPath $sourcePdf }
    $htmlUrl = ([uri](Join-Path $PSScriptRoot 'nature_presentation.html')).AbsoluteUri
    $profile = Join-Path $work 'edge_profile'
    $edgeArguments = @(
        '--headless', '--disable-gpu', '--no-pdf-header-footer',
        '--no-first-run', '--no-default-browser-check',
        "--user-data-dir=`"$profile`"", "--print-to-pdf=`"$sourcePdf`"", "`"$htmlUrl`""
    )
    Start-Process -FilePath $edge -ArgumentList $edgeArguments -WindowStyle Hidden -Wait
    if (!(Test-Path -LiteralPath $sourcePdf)) { throw 'Browser PDF rendering failed.' }

    & $poppler -r 96 -png $sourcePdf (Join-Path $work 'page')
    if ($LASTEXITCODE -ne 0) { throw 'PDF page inspection failed.' }

    # Inspect raster previews to find whitespace; keep the original PDF content sharp.
    @'
from pathlib import Path
from copy import copy
from PIL import Image, ImageChops
from pypdf import PdfReader, PdfWriter, PageObject, Transformation

work = Path('tmp/pdfs/render')
reader = PdfReader(work / 'pages.pdf')
regions = []
for i, page in enumerate(reader.pages, 1):
    image = Image.open(work / f'page-{i}.png').convert('RGB')
    difference = ImageChops.difference(image, Image.new('RGB', image.size, 'white')).convert('L')
    bounds = difference.point(lambda x: 255 if x > 15 else 0).getbbox()
    if bounds:
        scale = float(page.mediabox.height) / image.height
        lower = max(0, (image.height - bounds[3]) * scale - 3)
        upper = min(float(page.mediabox.height), (image.height - bounds[1]) * scale + 3)
        regions.append((page, lower, upper))

width = float(reader.pages[0].mediabox.width)
gap = 18
height = sum(upper - lower for _, lower, upper in regions) + gap * (len(regions) + 1)
canvas = PageObject.create_blank_page(width=width, height=height)
cursor = height - gap
for original, lower, upper in regions:
    page = copy(original)
    page.cropbox.lower_left = (0, lower)
    page.cropbox.upper_right = (width, upper)
    canvas.merge_transformed_page(page, Transformation().translate(ty=cursor - upper))
    cursor -= upper - lower + gap

writer = PdfWriter()
writer.add_page(canvas)
writer.add_metadata({'/Title': reader.metadata.title or 'Continuous presentation'})
output = Path('nature_presentation_continuous.pdf')
with output.open('wb') as stream:
    writer.write(stream)
assert len(PdfReader(output).pages) == 1
print(f'Saved: {output.resolve()}')
'@ | & $python -
    if ($LASTEXITCODE -ne 0) { throw 'Continuous PDF creation failed.' }
} finally {
    Pop-Location
}
