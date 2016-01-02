#version 430 core

layout (binding = 0) uniform sampler2D tex;

uniform float gamma = 2.2f;

in VS_OUT
{
    vec2 uv;
} fs_in;

out vec4 color;

void main(void)
{
    vec4 tex_color = texture(tex, fs_in.uv);
    vec3 correct = pow(tex_color.rgb, vec3(1.0f / gamma));
    color = vec4(correct, tex_color.a);
}
