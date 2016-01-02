#version 430 core

layout (binding = 0) uniform sampler2D tex_sprite;

in vec2 uv;

out vec4 color;

void main(void)
{
    vec2 inv_uv = vec2(uv.x, 1 - uv.y);
    color = texture(tex_sprite, inv_uv);
}
