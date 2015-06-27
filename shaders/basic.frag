#version 430 core

layout (binding = 0) uniform sampler2D tex_object;

in VS_OUT
{
    vec3 position;
    vec2 tc;
} fs_in;

out vec4 color;

void main(void)
{
    vec4 tex_color = texture(tex_object, fs_in.tc);
    vec4 pos_color = vec4((fs_in.position + 1.0) / 2.0, 1.0);
    color = mix(tex_color, pos_color, 0.8);
}
