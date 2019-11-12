/*
 * replace \ to / fast
int main(int argc, char* argv[])
{
	FILE* fr, *fw;
	fopen_s(&fr, argv[1], "rb");
	struct stat statbuf{};
	stat(argv[1], &statbuf);
	const auto len = statbuf.st_size;
	const auto buf = static_cast<uint8_t*>(malloc(sizeof(uint8_t) * len));
	fread(buf, sizeof(uint8_t), len, fr);
	fclose(fr);
	for (auto i = 0; i < len; ++i) if (buf[i] == '\\') buf[i] = '/';
	const auto pathlen = strlen(argv[1]);
	const auto output = static_cast<char*>(malloc(sizeof(uint8_t) * pathlen + 15));
	strcpy_s(output, pathlen + 15, argv[1]);
	strcat_s(output, pathlen + 15, ".replaced");
	fopen_s(&fw, output, "wb");
	fwrite(buf, sizeof(uint8_t), len, fw);
	fclose(fw);
}
*/

/*
 * bit swap
#include <iostream>

template<typename T, std::size_t... N>
constexpr T bswap_impl(T i, std::index_sequence<N...>)
{
	return ((((i >> (N * CHAR_BIT)) & static_cast<T>(static_cast<unsigned char>(-1))) << ((sizeof(T) - 1 - N) * CHAR_BIT)) | ...);
};
template<typename T, typename U = typename std::make_unsigned<T>::type>
constexpr U bswap(T i)
{
	return bswap_impl<U>(i, std::make_index_sequence<sizeof(T)>{});
}

int main(int argc, char* argv[])
{
	std::cout << std::hex << bswap<std::uint16_t>(0x1234u);
	system("pause");
}
*/

/*
 * opengl hello world
#include <GL/gl3w.h>
#include <GLFW/glfw3.h>
#include <ostream>
#include <iostream>

//#include <GL3W/glcorearb.h>
//#include <gl/GL.h>

#pragma comment(lib, "glfw3.lib")

typedef struct {
	GLenum       type;
	const char*  filename;
	GLuint       shader;
} ShaderInfo;

GLuint LoadShaders(ShaderInfo*);
#define BUFFER_OFFSET(a) ((void*)(a))

enum VAOIDs { Triangles, VAONum };
enum BufferIDs { ArrayBuffer, BufferNum };
enum AttribIDs { vPosition = 0 };

GLuint VAOs[VAONum];
GLuint Buffers[BufferNum];

const GLuint NumVertices = 6;


static const GLchar* ReadShader(const char* filename)
{
#ifdef WIN32
	FILE* infile;
	fopen_s(&infile, filename, "rb");
#else
	FILE* infile = fopen(filename, "rb");
#endif // WIN32

	if (!infile) {
		return NULL;
	}

	fseek(infile, 0, SEEK_END);
	int len = ftell(infile);
	fseek(infile, 0, SEEK_SET);

	GLchar* source = new GLchar[len + 1];

	fread(source, 1, len, infile);
	fclose(infile);

	source[len] = 0;

	return const_cast<const GLchar*>(source);
}

GLuint LoadShaders(ShaderInfo* shaders)
{
	if (shaders == NULL) { return 0; }

	GLuint program = glCreateProgram();

	ShaderInfo* entry = shaders;
	while (entry->type != GL_NONE) {
		GLuint shader = glCreateShader(entry->type);

		entry->shader = shader;

		const GLchar* source = ReadShader(entry->filename);
		if (source == NULL) {
			for (entry = shaders; entry->type != GL_NONE; ++entry) {
				glDeleteShader(entry->shader);
				entry->shader = 0;
			}

			return 0;
		}

		glShaderSource(shader, 1, &source, NULL);
		delete[] source;

		glCompileShader(shader);

		GLint compiled;
		glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
		if (!compiled) {


			return 0;
		}

		glAttachShader(program, shader);

		++entry;
	}

	glLinkProgram(program);

	GLint linked;
	glGetProgramiv(program, GL_LINK_STATUS, &linked);
	if (!linked) 
	{
		for (entry = shaders; entry->type != GL_NONE; ++entry)
		{
			glDeleteShader(entry->shader);
			entry->shader = 0;
		}
		return 0;
	}
	return program;
}


int main(int argc, char* argv[])
{
	glfwInit();
	GLFWwindow* window = glfwCreateWindow(640, 480, "Fuck OpenGL", NULL, NULL);
	glfwMakeContextCurrent(window);
	gl3wInit();
	const GLfloat vertices[NumVertices][2] = {
		-0.9, -0.9, 0.85,
		-0.9, -0.9, 0.85,
		0.9, -0.85, 0.9,
		0.9, -0.85, 0.9
	};
	glCreateBuffers(BufferNum, Buffers);
	glNamedBufferStorage(Buffers[ArrayBuffer], sizeof(vertices), vertices, 0);
	ShaderInfo shaders[] = { {GL_VERTEX_SHADER,"tri.vert"},{GL_FRAGMENT_SHADER,"tri.frag"},{GL_NONE,NULL} };
	GLuint program = LoadShaders(shaders);
	glUseProgram(program);
	glGenVertexArrays(VAONum, VAOs);
	glBindVertexArray(VAOs[Triangles]);
	glBindBuffer(GL_ARRAY_BUFFER, Buffers[ArrayBuffer]);
	glVertexAttribPointer(vPosition, 2, GL_FLOAT, GL_FALSE, 0, BUFFER_OFFSET(0));
	glEnableVertexAttribArray(vPosition);
	while (!glfwWindowShouldClose(window))
	{
		const float black[] = { 0,0,0,0 };
		glClearBufferfv(GL_COLOR, 0, black);
		glBindVertexArray(VAOs[Triangles]);
		glDrawArrays(GL_TRIANGLES, 0, NumVertices);
		glfwSwapBuffers(window);
		glfwPollEvents();
	}
	glfwDestroyWindow(window);
	glfwTerminate();
}
*/

