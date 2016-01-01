#version 430 core

layout (binding = 0) uniform sampler2D tex_diffuse;

in VS_OUT
{
    vec2 uv;
    vec4 color;
} fs_in;

out vec4 color;

void main(void)
{
    vec2 uv = vec2(fs_in.uv.x, 1 - fs_in.uv.y);
    vec4 tex_color = texture(tex_diffuse, uv);
    color = fs_in.color * tex_color;
}
