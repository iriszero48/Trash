#pragma once

#include <cstdint>
#include <filesystem>

#include <eigen3/Eigen/Eigen>
#include <opencv2/opencv.hpp>

#include "Cryptography.h"
#include "OpenCvUtility.h"
#include "String.h"

class ImageDatabase
{
public:
	struct Image
	{
		std::filesystem::path Path{};
		std::string Md5{};
		Eigen::Matrix<float, 512, 1> Vgg16;

		explicit Image() = default;

		// Deserialization
		explicit Image(const uint8_t* data, const uint64_t pathLen)
		{
			Path = std::filesystem::u8path(std::string((char*)data, pathLen));
			Md5 = std::string_view((char*)data + pathLen, 32);
			Vgg16 = Eigen::Matrix<float, 512, 1>((float*)(data + pathLen + 32));
		}

		explicit Image(const std::filesystem::path& path, const std::string& data): Path(path), data(data)
		{
			
		}
		
		explicit Image(std::filesystem::path path) : Path(std::move(path))
		{
			data = OpenCvUtility::ReadToEnd(Path).value();
		}

	private:
		std::string data{};

	public:
		void ComputeVgg16()
		{
			static std::string ProtoTxt = OpenCvUtility::ReadToEnd(R"(vgg16-deploy.prototxt)").value();
			static std::string CaffeModel = OpenCvUtility::ReadToEnd(R"(vgg16.caffemodel)").value();

			static auto vgg16 = []()
			{
				auto vgg16 = cv::dnn::readNetFromCaffe(
					ProtoTxt.data(), ProtoTxt.length(),
					CaffeModel.data(), CaffeModel.length());
				vgg16.setPreferableBackend(cv::dnn::DNN_BACKEND_CUDA);
				vgg16.setPreferableTarget(cv::dnn::DNN_TARGET_CUDA);
				return vgg16;
			}();
			vgg16.setInput(OpenCvUtility::ReadBlob(data.data(), data.length()));
			auto out = vgg16.forward();
			out = out / norm(out);
			for (int i = 0; i < 512; ++i)
			{
				Vgg16(i, 0) = out.at<float>(0, i, 0);
			}
			//std::cout << *Convert::ToString(Vgg16(0, 0)) << std::endl;
		}

		void ComputeMd5()
		{
			try
			{
				Cryptography::Md5 md5;
				md5.Append((std::uint8_t*)data.data(), data.length());
				Md5 = md5.Digest();
			}
			catch (...)
			{
				Md5 = std::string(32, 0);
			}
		}

		void Serialization(std::ofstream& fs)
		{
			const auto p = Path.u8string();
			const uint64_t pl = p.length();
			auto pls = *Convert::ToString(pl, 16);
			String::PadLeft(pls, 16, '0');
			fs.write(pls.data(), pls.length());
			fs.write(p.data(), p.length());
			fs.write(Md5.data(), 32);
			fs.write((char*)Vgg16.data(), 512 * sizeof(float));
		}

		void FreeMemory()
		{
			data = {};
		}
	};

	ImageDatabase(std::filesystem::path path) : path(std::move(path))
	{

	}

	void Load(const std::filesystem::path& path)
	{
		std::ifstream fs(path, std::ios::in | std::ios::binary);
		if (!fs) throw std::runtime_error("load file: bad stream");
		const auto fsbuf = std::make_unique<char[]>(4096);
		fs.rdbuf()->pubsetbuf(fsbuf.get(), 4096);

		while (!fs.eof())
		{
			char lenBuf[16 + 1]{0};
			fs.read(lenBuf, 16);
			if (fs.gcount() == 0) break;
			const auto pathLen = *Convert::FromString<uint64_t>(std::string(lenBuf, 16), 16);
			const auto len = pathLen + 32u + sizeof(float) * 512u;
			std::string buf(len, 0);
			fs.read(&buf[0], len);
			Images.push_back(Image((uint8_t*)buf.data(), pathLen));
		}

		fs.close();
	}

	void Save(const std::filesystem::path& path)
	{
		std::filesystem::remove(path);
		
		std::ofstream fs(path, std::ios::out | std::ios::binary);
		if (!fs) throw std::runtime_error("load file: bad stream");
		const auto fsbuf = std::make_unique<char[]>(4096);
		fs.rdbuf()->pubsetbuf(fsbuf.get(), 4096);

		for (auto img : Images)
		{
			img.Serialization(fs);
		}

		fs.close();
	}
private:
	std::filesystem::path path;

public:
	std::vector<Image> Images;
};
