const { chromium } = require('C:/Program Files/WindowsApps/OpenAI.CodexPrimaryRuntime.v26-1119-907-0_26.1119.907.0_x64__3k8sg7r9htsxt/dependencies/node/node_modules/playwright');
const {pathToFileURL} = require('url');
const path = require('path');
(async()=>{
 const browser=await chromium.launch({executablePath:'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe',headless:true});
 const page=await browser.newPage();
 await page.goto(pathToFileURL(path.resolve('Supplementary_Information.html')).href,{waitUntil:'networkidle'});
 await page.evaluate(async()=>{await document.fonts.ready;await Promise.all([...document.images].map(i=>i.decode()));});
 await page.addStyleTag({content:'@media print { @page { size:A4; margin:12mm; } html,body{width:auto!important;} .main-container{width:auto!important;padding:0!important;} .figure {break-inside:avoid; page-break-inside:avoid;} .figure img{width:auto!important;max-width:100%!important;max-height:245mm;object-fit:contain;} .figure .caption{break-after:avoid;} thead{display:table-header-group;} tr{break-inside:avoid;} }'});
 await page.pdf({path:'Supplementary_Information.pdf',format:'A4',printBackground:true,preferCSSPageSize:true});
 await browser.close();
})();
