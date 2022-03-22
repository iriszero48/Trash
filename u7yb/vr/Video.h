#pragma once

#include <cstdint>
#include <filesystem>
#include <utility>
#include <vector>
#include <array>
#include <unordered_set>

extern "C"
{
#include <libavformat/avformat.h>
#include <libavcodec/avcodec.h>
#include <libavutil/avutil.h>
#include <libavutil/imgutils.h>
#include <libavutil/opt.h>
#include <libswscale/swscale.h>
}

#include "String.h"

namespace Video
{
	class Exception : public std::runtime_error
	{
	public:
		template<typename...Args>
		Exception(Args&&... str): std::runtime_error(String::StringCombineStr(std::forward<Args>(str)...)){}
	};

	namespace __Detail
	{
		std::string FFmpegErrStr(const int err)
		{
			char buf[4096]{0};
			if (const auto ret = av_strerror(err, buf, 4096); ret < 0)
				return String::FormatStr("unknow error code: {}", ret);
			return std::string(buf);
		}

		template <typename T, size_t S>
		constexpr const char* GetFilename(const T (& str)[S], size_t i = S - 1)
		{
			for (; i > 0; --i) if (str[i] == '/') return &str[i + 1];
			return str;
		}
	}

#define Except(...) Exception( \
	"[", funcName, "] ",\
	"[",__Detail::GetFilename(__FILE__),":",std::to_string(__LINE__),"] ",\
	String::FormatStr(__VA_ARGS__))
#define ThrowEx(...) throw Except(__VA_ARGS__)

    class Video
    {
    public:
        bool die = false;

        std::filesystem::path Path{};

        Video() = delete;

        explicit Video(std::string path) : Path(path) {}

        void Parse()
        {
            LoadFile(Path);
        }

        void SetLog(void(*callback)(const std::string&))
        {
            log = callback;
        }

    private:
        void(*log)(const std::string&) = nullptr;

        void LoadFile(const std::filesystem::path& path)
        {
			constexpr auto funcName = "Video::Video::LoadFile";

            AVFormatContext* fmtCtx = nullptr;
			AVDictionary *decOpt = nullptr;
			av_dict_set(&decOpt, "strict", "1", 0);
			av_dict_set(&decOpt, "timeout", "300000", 0);
            if (const int ret = avformat_open_input(&fmtCtx, path.u8string().c_str(), nullptr, &decOpt); ret < 0)
				ThrowEx("avforamt_open_input: {}",  __Detail::FFmpegErrStr(ret));
			LoadVideo(path, fmtCtx);

            if (fmtCtx) avformat_close_input(&fmtCtx);
        }

        void LoadVideo(const std::filesystem::path& path, AVFormatContext* fmtCtx) const
        {
			constexpr auto funcName = "Video::Video::LoadVideo";

            AVCodec* codec = nullptr;
            AVCodecContext* codecCtx = nullptr;
            AVFrame* frame = nullptr;
            AVFrame* decFrame = nullptr;
            SwsContext* swsCtx = nullptr;

            try
            {
                int ret = avformat_find_stream_info(fmtCtx, nullptr);
                if (ret < 0) ThrowEx("avformat_find_stream_info: {}", __Detail::FFmpegErrStr(ret));

                ret = av_find_best_stream(fmtCtx, AVMEDIA_TYPE_VIDEO, -1, -1, &codec, 0);
                if (ret < 0) ThrowEx("av_find_best_stream: {}", __Detail::FFmpegErrStr(ret));

                const int streamId = ret;
                const auto codecParams = fmtCtx->streams[streamId]->codecpar;
                //const auto until = (int)(av_q2d(fmtCtx->streams[streamId]->r_frame_rate) * 30);
                const auto until = 24 * 30;

                codecCtx = avcodec_alloc_context3(codec);
                if (!codecCtx) ThrowEx("avcodec_alloc_context3: NULL");

                ret = avcodec_parameters_to_context(codecCtx, codecParams);
                if (ret < 0) ThrowEx("avcodec_parameters_to_context: {}", __Detail::FFmpegErrStr(ret));

                av_opt_set(codecCtx, "strict", "1", 0);
				av_opt_set(codecCtx, "timeout", "300000", 0);

				AVDictionary *decOpt = nullptr;
				av_dict_set(&decOpt, "strict", "1", 0);
				av_dict_set(&decOpt, "timeout", "300000", 0);
                ret = avcodec_open2(codecCtx, codec, &decOpt);
				av_dict_free(&decOpt);
                if (ret < 0) ThrowEx("avcodec_open2: {}", __Detail::FFmpegErrStr(ret));

                constexpr auto dstWidth = 100;
                constexpr auto dstHeight = 100;
                constexpr AVPixelFormat dstPixFmt = AV_PIX_FMT_BGR24;
                bool setRange = false;

//                if (codecCtx->pix_fmt != AV_PIX_FMT_NONE)
//                {
//                    swsCtx = sws_getCachedContext(
//                            nullptr, codecParams->width, codecParams->height, codecCtx->pix_fmt,
//                            dstWidth, dstHeight, dstPixFmt, 0, nullptr, nullptr, nullptr);
//                    if (!swsCtx) throw Exception("[ImageDatabase::ImageFile::LoadImage] [sws_getCachedContext] NULL");
//                }

                frame = av_frame_alloc();
                if (!frame) ThrowEx("av_frame_alloc: NULL");

                const auto frameSize = av_image_get_buffer_size(dstPixFmt, dstWidth, dstHeight, dstWidth);
                if (frameSize < 0) ThrowEx("av_image_get_buffer_size: {}", __Detail::FFmpegErrStr(frameSize));

                std::string frameBuf(frameSize, 0);
                ret = av_image_fill_arrays(
                        frame->data, frame->linesize,
                        reinterpret_cast<uint8_t*>(frameBuf.data()),
                        dstPixFmt, dstWidth, dstHeight, dstWidth);
                if (ret < 0) ThrowEx("av_image_fill_arrays: {}", __Detail::FFmpegErrStr(ret));

                decFrame = av_frame_alloc();
                if (!decFrame) ThrowEx("av_frame_alloc: NULL");

                bool eof = false;
                AVPacket* pkt = av_packet_alloc();
                if (!pkt) ThrowEx("av_packet_alloc: NULL");

                uint64_t index = 0;

                do
                {
                    if (!eof)
                    {
                        ret = av_read_frame(fmtCtx, pkt);
                        if (ret < 0 && ret != AVERROR_EOF) ThrowEx("av_read_frame: {}", __Detail::FFmpegErrStr(ret));

                        if (ret == 0 && pkt->stream_index != streamId)
                        {
                            av_packet_unref(pkt);
                            continue;
                        }
                        eof = (ret == AVERROR_EOF);
                    }
                    if (eof)
                    {
                        av_packet_unref(pkt);
                        ret = 0;
                    }
                    else
                    {
                        ret = avcodec_send_packet(codecCtx, pkt);
                        if (ret < 0) ThrowEx("avcodec_send_packet: {}", __Detail::FFmpegErrStr(ret));
                    }
                    while (ret >= 0)
                    {
                        ret = avcodec_receive_frame(codecCtx, decFrame);
                        if (ret == AVERROR(EAGAIN) || ret == AVERROR_EOF)
                        {
                            av_packet_unref(pkt);
                            break;
                        }
                        if (ret < 0) ThrowEx("avcodec_receive_frame: {}", __Detail::FFmpegErrStr(ret));
//                        if (swsCtx == nullptr)
//                        {
//                            swsCtx = sws_getCachedContext(
//                                    nullptr, codecParams->width, codecParams->height, codecCtx->pix_fmt,
//                                    dstWidth, dstHeight, dstPixFmt, 0, nullptr, nullptr, nullptr);
//                            if (!swsCtx) throw Exception("[ImageDatabase::ImageFile::LoadImage] [sws_getCachedContext] NULL");
//                        }
//
//                        sws_scale(swsCtx, decFrame->data, decFrame->linesize, 0, decFrame->height, frame->data, frame->linesize);

                        //messageQueue.Write(ProcImage(index == 0 ? path : path / *Convert::ToString(index), frameBuf, frame->linesize[0]));
                        index++;
                        if (die || index > until)
                        {
                            av_packet_unref(pkt);
                            av_packet_free(&pkt);
                            goto end;
                        }
                    }
                    av_packet_unref(pkt);
                } while (!eof);
                av_packet_free(&pkt);
            }
            catch (...)
            {
                av_frame_free(&decFrame);
                av_frame_free(&frame);
                avcodec_free_context(&codecCtx);
                sws_freeContext(swsCtx);
                std::throw_with_nested(Except("load video failed"));
            }

            end:
            av_frame_free(&decFrame);
            av_frame_free(&frame);
            avcodec_free_context(&codecCtx);
            sws_freeContext(swsCtx);
        }
    };
}

#undef Except
#undef ThrowEx
