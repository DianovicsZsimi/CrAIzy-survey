from pathlib import Path
from pypdf import PdfReader
from PIL import Image,ImageDraw
import re
r=PdfReader('Supplementary_Information.pdf');text='\n'.join(p.extract_text() for p in r.pages)
nums=list(map(int,re.findall(r'(?m)^S(\d+)\s*$',text)))
assert nums==list(range(1,44)),nums
print('Verified S1-S43 across',len(r.pages),'pages')
paths=sorted(Path('tmp/pdfs').glob('page-*.png'))
for batch in range((len(paths)+11)//12):
 sheet=Image.new('RGB',(840,1320),'#cccccc');draw=ImageDraw.Draw(sheet)
 for k,p in enumerate(paths[batch*12:(batch+1)*12]):
  im=Image.open(p);im.thumbnail((270,305));x=k%3*280;y=k//3*330
  sheet.paste(im,(x,y+20));draw.text((x+5,y+3),p.stem,fill='black')
 sheet.save(f'tmp/pdfs/contact-{batch}.png')
print('Sheets:',(len(paths)+11)//12)
