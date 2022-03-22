#pragma once

#include <filesystem>

#include <mupdf/fitz.h>

#include "JsonDevice.h"
#include "String.h"
#include "Macro.h"

class Pdf2Json
{
private:
    fz_context *ctx;
	fz_document *doc;
    uint64_t page_count;
    uint64_t page_number;
    fz_page* pageCtx;
    fz_rect pageSize;

public:
    Pdf2Json() = delete;

    Pdf2Json(const std::filesystem::path& path, const uint64_t page): page_number(page)
    {
	    ctx = fz_new_context(NULL, NULL, FZ_STORE_UNLIMITED);
	    if (!ctx) throw std::runtime_error("cannot create mupdf context");

	    fz_try(ctx)
		    fz_register_document_handlers(ctx);
	    fz_catch(ctx)
	    {
            const auto msg = String::FormatNew<std::string>("cannot register document handlers: {}", fz_caught_message(ctx));
		    fz_drop_context(ctx);
            throw std::runtime_error(msg);
	    }

        fz_try(ctx)
            doc = fz_open_document(ctx, path.u8string().c_str());
        fz_catch(ctx)
        {
            const auto msg = String::FormatNew<std::string>("cannot open document: {}", fz_caught_message(ctx));
            fz_drop_context(ctx);
            throw std::runtime_error(msg);
        }

        fz_try(ctx)
            page_count = fz_count_pages(ctx, doc);
        fz_catch(ctx)
        {
            const auto msg = String::FormatNew<std::string>("cannot count number of pages: {}", fz_caught_message(ctx));
            fz_drop_document(ctx, doc);
            fz_drop_context(ctx);
            throw std::runtime_error(msg);
        }
        
        if (page >= page_count)
        {
            fz_drop_document(ctx, doc);
            fz_drop_context(ctx);
            throw std::runtime_error(String::FormatNew<std::string>("page number out of range: {} (page count {})", page + 1, page_count));
        }

        pageCtx = fz_load_page(ctx, doc, page_number);
        pageSize = fz_bound_page(ctx, pageCtx);
    }

    std::filesystem::path SaveAsJson(const std::filesystem::path& outdir)
    {
        if (!std::filesystem::exists(outdir)) std::filesystem::create_directory(outdir);
        const std::filesystem::path filename =
            outdir / String::FormatNew<decltype(std::filesystem::path().u8string())>("{}.json", std::this_thread::get_id);
        auto out = fz_new_output_with_path(ctx, filename.u8string().c_str(), 0);
        auto dev = JsonDevice(ctx, out, pageSize.x1 - pageSize.x0, pageSize.y1 - pageSize.y0);
        fz_try(ctx)
        {
            fz_run_page(ctx, pageCtx, dev.Get(), fz_identity, NULL);
        }
        fz_always(ctx)
        {
            dev.Close();
            fz_close_output(ctx, out);
            fz_drop_output(ctx, out);
        }
        fz_catch(ctx)
        {
            throw std::runtime_error(String::FormatNew<std::string>("error: {}: {}", MacroFunctionName, fz_caught_message(ctx)));
        }
        return filename;
    }

    std::filesystem::path SaveAsSvg(const std::filesystem::path& outdir)
    {
        if (!std::filesystem::exists(outdir)) std::filesystem::create_directory(outdir);
        const std::filesystem::path filename =
            outdir / String::FormatNew<decltype(std::filesystem::path().u8string())>("{}.svg", std::this_thread::get_id);
        auto out = fz_new_output_with_path(ctx, filename.u8string().c_str(), 0);
        auto dev = fz_new_svg_device(ctx, out, pageSize.x1 - pageSize.x0, pageSize.y1 - pageSize.y0, 1, 1);
        fz_try(ctx)
        {
            fz_run_page(ctx, pageCtx, dev, fz_identity, NULL);
        }
        fz_always(ctx)
        {
            fz_close_device(ctx, dev);
            fz_close_output(ctx, out);
            fz_drop_device(ctx, dev);
            fz_drop_output(ctx, out);
        }
        fz_catch(ctx)
        {
            throw std::runtime_error(String::FormatNew<std::string>("error: {}: {}", MacroFunctionName, fz_caught_message(ctx)));
        }
        return filename;
    }

    ~Pdf2Json()
    {
        fz_drop_document(ctx, doc);
        fz_drop_context(ctx);
    }
};