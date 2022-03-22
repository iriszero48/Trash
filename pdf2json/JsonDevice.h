#pragma once

#include <mupdf/fitz.h>

#include <string.h>
#include <float.h>
#include <math.h>

struct Tile
{
	int pattern;
	fz_matrix ctm;
	fz_rect view;
	fz_rect area;
	fz_point step;
};

struct Glyph
{
	float x_off;
	float y_off;
};

struct Font
{
	int id;
	fz_font *font;
	int max_sentlist;
	glyph *sentlist;
};

struct Image
{
	int id;
	fz_image *image;
};

struct JsonDev
{
	fz_device super;

	fz_output *out;
	fz_output *out_store;
	fz_output *defs;
	fz_buffer *defs_buffer;
	int def_count;

	int blend_bitmask;

	int num_tiles;
	int max_tiles;
	Tile *tiles;

	int num_fonts;
	int max_fonts;
	Font *fonts;

	int num_images;
	int max_images;
	Image *images;

	int layers;
};


namespace __Detail
{
    static unsigned int JsonHexColor(fz_context *ctx, fz_colorspace *colorspace, const float *color, fz_color_params color_params)
    {
        float rgb[3];
        int r, g, b;

        if (colorspace != fz_device_rgb(ctx))
        {
            fz_convert_color(ctx, colorspace, color, fz_device_rgb(ctx), rgb, NULL, color_params);
            color = rgb;
        }

        r = fz_clampi(255 * color[0] + 0.5f, 0, 255);
        g = fz_clampi(255 * color[1] + 0.5f, 0, 255);
        b = fz_clampi(255 * color[2] + 0.5f, 0, 255);

        return (r << 16) | (g << 8) | b;
    }

    static void JsonDevFillColor(fz_context *ctx, JsonDev *sdev, fz_colorspace *colorspace, const float *color, float alpha, fz_color_params color_params)
    {
        fz_output *out = sdev->out;
        int rgb = svg_hex_color(ctx, colorspace, color, color_params);
        fz_write_printf(ctx, out, ",%q:\"%06x\"", "color", rgb);
        if (alpha != 1) fz_write_printf(ctx, out, ",%q:%g", "opacity", alpha);
    }

    static void JsonDevFontFamily(fz_context *ctx, char buf[], int size, const char *name)
    {
        auto *p = strchr(name, '+');
        if (p) fz_strlcpy(buf, p+1, size);
        else fz_strlcpy(buf, name, size);
        {
            auto* p = strrchr(buf, '-');
            if (p) *p = 0;
        }
    }

    static int JsonDevFindNextLineBreak(fz_context *ctx, const fz_text_span *span, fz_matrix inv_tm, int i)
    {
        fz_point p, old_p;

        old_p.x = span->items[i].x;
        old_p.y = span->items[i].y;
        old_p = fz_transform_point(old_p, inv_tm);

        for (++i; i < span->len; ++i)
        {
            if (span->items[i].ucs >= 0)
            {
                p.x = span->items[i].x;
                p.y = span->items[i].y;
                p = fz_transform_point(p, inv_tm);
                if (span->wmode == 0)
                {
                    if (p.y != old_p.y)
                        return i;
                }
                else
                {
                    if (p.x != old_p.x)
                        return i;
                }
                old_p = p;
            }
        }

        return i;
    }

    static float JsonDevClusterAdvance(fz_context *ctx, const fz_text_span *span, int i, int end)
    {
        int n = 1;
        while (i + n < end && span->items[i + n].gid == -1)
            ++n;
        if (n > 1)
            return fz_advance_glyph(ctx, span->font, span->items[i].gid, span->wmode) / n;
        return 0;
    }

    static void JsonDevTextSpan(fz_context *ctx, JsonDev *sdev, fz_matrix ctm, const fz_text_span *span)
    {
        fz_output *out = sdev->out;
        char font_family[100];
        int is_bold, is_italic;
        fz_matrix tm, inv_tm, final_tm;
        fz_point p;
        float font_size;
        fz_text_item *it;
        int start, end, i;
        float cluster_advance = 0;

        if (span->len == 0)
        {
            fz_write_printf(ctx, out, "},");
            return;
        }

        tm = span->trm;
        font_size = fz_matrix_expansion(tm);
        final_tm.a = tm.a / font_size;
        final_tm.b = tm.b / font_size;
        final_tm.c = -tm.c / font_size;
        final_tm.d = -tm.d / font_size;
        final_tm.e = 0;
        final_tm.f = 0;
        inv_tm = fz_invert_matrix(final_tm);
        final_tm = fz_concat(final_tm, ctm);

        tm.e = span->items[0].x;
        tm.f = span->items[0].y;

        JsonDevFontFamily(ctx, font_family, sizeof font_family, fz_font_name(ctx, span->font));
        is_bold = fz_font_is_bold(ctx, span->font);
        is_italic = fz_font_is_italic(ctx, span->font);

        fz_write_printf(ctx, out, ",%q:\"%M\"", "transform", &final_tm);
        fz_write_printf(ctx, out, ",%q:{%q:\"%g\"", "font", "size", font_size);
        fz_write_printf(ctx, out, ",%q:%q", "family", font_family);
        if (is_bold) fz_write_printf(ctx, out, ",\"weight\":\"bold\"");
        if (is_italic) fz_write_printf(ctx, out, ",\"style\":\"italic\"");
        if (span->wmode != 0) fz_write_printf(ctx, out, ",\"wmode\":\"tb\"");

        fz_write_printf(ctx, out, "},\"lines\":[");

        start = find_first_char(ctx, span, 0);
        while (start < span->len)
        {
            end = JsonDevFindNextLineBreak(ctx, span, inv_tm, start);

            p.x = span->items[start].x;
            p.y = span->items[start].y;
            p = fz_transform_point(p, inv_tm);
            if (span->items[start].gid >= 0)
                cluster_advance = JsonDevClusterAdvance(ctx, span, start, end);
            fz_write_printf(ctx, out, "{\"x\":\"%g\",\"y\":\"%g\",\"text\":\"", p.x, p.y);
            for (i = start; i < end; ++i)
            {
                it = &span->items[i];
                if (it->ucs >= 0)
                {
                    int c = it->ucs;
                    if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'))
                        fz_write_byte(ctx, out, c);
                    else
                        fz_write_printf(ctx, out, "\\u%04x", c);
                }
            }
            start = find_first_char(ctx, span, end);
            if (start < span->len)
            {
                fz_write_printf(ctx, out, "\"},");
            }
            else
            {
                fz_write_printf(ctx, out, "\"}");
            }
        }

        fz_write_printf(ctx, out, "]},");
    }

    static void svg_dev_ctm(fz_context *ctx, JsonDev *jdev, fz_matrix ctm)
    {
        fz_output *out = jdev->out;

        if (ctm.a != 1.0f || ctm.b != 0 || ctm.c != 0 || ctm.d != 1.0f || ctm.e != 0 || ctm.f != 0)
        {
            fz_write_printf(ctx, out, " transform=\"matrix(%g,%g,%g,%g,%g,%g)\"",
                ctm.a, ctm.b, ctm.c, ctm.d, ctm.e, ctm.f);
        }
    }
}

class JsonDevice
{
private:
    fz_context* ctx;
    JsonDev* dev;
public:
    JsonDevice() = delete;
    JsonDevice(fz_context* ctx, fz_output* out, float page_width, float page_height) : ctx(ctx) 
    {
        JsonDev* dev = fz_new_derived_device(ctx, JsonDev);
        dev->super.close_device = JsonDevCloseDevice;
        dev->super.drop_device = JsonDevDropDevice;

        dev->super.fill_text = JsonDevFillText;
        dev->super.stroke_text = JsonDevStrokeText;
        dev->super.ignore_text = JsonDevIgnoreText;

        dev->out = out;
        dev->out_store = out;
        dev->layers = 0;

        fz_write_printf(ctx, out, "{\"w\":\"%g\",\"h\":\"%g\",\"blocks\":[",
            page_width, page_height);
        this->dev = dev;
    };
    fz_device* Get() { return (fz_device*)dev; }
    void Close()
    {
        fz_close_device(ctx, (fz_device*)dev);
        fz_drop_device(ctx, (fz_device*)dev);
    }
    ~JsonDevice() = default;
private:
    static void JsonDevCloseDevice(fz_context *ctx, fz_device *dev)
    {
        JsonDev *jdev = (JsonDev*)dev;
        fz_output *out = jdev->out;
        fz_write_printf(ctx, out, "]}");
    }

    static void JsonDevDropDevice(fz_context *ctx, fz_device *dev)
    {
        JsonDev *jdev = (JsonDev*)dev;
        int i;

        fz_free(ctx, jdev->tiles);
        fz_drop_buffer(ctx, jdev->defs_buffer);
        fz_drop_output(ctx, jdev->defs);
        for (i = 0; i < jdev->num_fonts; i++)
        {
            fz_drop_font(ctx, jdev->fonts[i].font);
            fz_free(ctx, jdev->fonts[i].sentlist);
        }
        fz_free(ctx, jdev->fonts);
        for (i = 0; i < jdev->num_images; i++)
        {
            fz_drop_image(ctx, jdev->images[i].image);
        }
        fz_free(ctx, jdev->images);
    }

    static void JsonDevFillText(
        fz_context *ctx, fz_device *dev,
        const fz_text *text, fz_matrix ctm,
	    fz_colorspace *colorspace,
        const float *color, float alpha, fz_color_params color_params)
    {
        auto *jdev = (JsonDev*)dev;
        fz_output *out = jdev->out;
        font *fnt;
        fz_text_span *span;

		for (span = text->head; span; span = span->next)
		{
            fz_write_printf(ctx, out, "{%q:%d", "type", 0);
			__Detail::JsonDevFillColor(ctx, jdev, colorspace, color, alpha, color_params);
            __Detail::JsonDevTextSpan(ctx, jdev, ctm, span);
		}
    }

    static void JsonDevStrokeText(fz_context *ctx, fz_device *dev,
        const fz_text *text, const fz_stroke_state *stroke,
        fz_matrix ctm, fz_colorspace *colorspace,
        const float *color, float alpha,
        fz_color_params color_params)
    {
        JsonDev *jdev = (JsonDev*)dev;
        fz_output *out = jdev->out;
        font *fnt;
        fz_text_span *span;

        for (span = text->head; span; span = span->next)
        {
            fz_write_printf(ctx, out, "{%q:%d", "type", 0);
            __Detail::JsonDevFillColor(ctx, jdev, colorspace, color, alpha, color_params);
            __Detail::JsonDevTextSpan(ctx, jdev, ctm, span);
        }
    }

    static void JsonDevIgnoreText(fz_context *ctx, fz_device *dev, const fz_text *text, fz_matrix ctm)
    {
        JsonDev *jdev = (JsonDev*)dev;
        fz_output *out = jdev->out;
        fz_text_span *span;

        float black[3] = { 0, 0, 0};

        for (span = text->head; span; span = span->next)
        {
            fz_write_printf(ctx, out, "{%q:%d", "type", 0);
            __Detail::JsonDevFillColor(ctx, jdev, fz_device_rgb(ctx), black, 0.0f, fz_default_color_params);
            __Detail::JsonDevTextSpan(ctx, jdev, ctm, span);
        }
    }

    static void svg_dev_fill_path(fz_context *ctx, fz_device *dev, const fz_path *path, int even_odd, fz_matrix ctm,
        fz_colorspace *colorspace, const float *color, float alpha, fz_color_params color_params)
    {
        svg_device *sdev = (svg_device*)dev;
        fz_output *out = sdev->out;

        fz_write_printf(ctx, out, "<path");
        svg_dev_ctm(ctx, sdev, ctm);
        svg_dev_path(ctx, sdev, path);
        svg_dev_fill_color(ctx, sdev, colorspace, color, alpha, color_params);
        if (even_odd)
            fz_write_printf(ctx, out, " fill-rule=\"evenodd\"");
        fz_write_printf(ctx, out, "/>\n");
    }

};
