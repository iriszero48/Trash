import pathlib
import fitz
import re

file = r"./*.pdf"
with fitz.open(file) as doc:
    with fitz.open() as out:
        out.insert_pdf(doc, from_page=0, to_page=0)
        out.save(f"{pathlib.Path(file).stem}.raw.pdf", expand=True)

    for page in doc:
        res_r = doc.xref_get_key(page.xref, "Resources/XObject/X1")[1]
        doc.xref_set_key(page.xref, "Resources/XObject", f"<< /X1 {res_r} >>")
        ctx_r = int(doc.xref_get_key(page.xref, "Contents")[1].split()[0])
        data = doc.xref_stream(ctx_r).decode("utf-8")
        data = re.sub(r"Q\nq(\n|.)*?/X2(\n|.)*?Q", "Q", data)
        doc.update_stream(ctx_r, data.encode("utf-8"))
        print(data)

    doc.save(file.replace('.pdf', ' - 修改.pdf'), garbage=4, deflate=True)

    with fitz.open() as out:
        out.insert_pdf(doc, from_page=0, to_page=0)
        out.save(f"{pathlib.Path(file).stem}.debug.pdf", expand=True)

    with fitz.open() as out:
        out.insert_pdf(doc, from_page=0, to_page=3)
        out.save(f"{pathlib.Path(file).stem}.rel.pdf", garbage=4, deflate=True)
