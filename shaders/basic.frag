#version 430 core

layout (binding = 0) uniform sampler2D tex_object;

in VS_OUT
{
    vec2 tc;
} fs_in;

out vec4 color;

void main(void)
{
    vec4 tex_color = texture(tex_object, fs_in.tc);
    color = tex_color;
}
