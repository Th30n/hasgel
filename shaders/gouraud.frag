#version 430 core

layout (binding = 0) uniform sampler2D tex_diffuse;

in VS_OUT
{
    vec2 uv;
    vec3 diff;
    vec3 spec;
} fs_in;

out vec4 color;

void main(void)
{
    vec2 uv = vec2(fs_in.uv.x, 1 - fs_in.uv.y);
    vec4 tex_color = texture(tex_diffuse, uv);
    vec3 diff = fs_in.diff * tex_color.rgb;
    color = vec4(diff + fs_in.spec, tex_color.a);
}
