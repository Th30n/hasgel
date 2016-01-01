#version 430 core

layout (binding = 0) uniform sampler2D tex;

in VS_OUT
{
    vec2 uv;
} fs_in;

out vec4 color;

void main(void)
{
    vec4 tex_color = texture(tex, fs_in.uv);
    color = tex_color;
}
