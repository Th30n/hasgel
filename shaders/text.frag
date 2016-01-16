#version 430 core

layout (binding = 0) uniform sampler2D tex_font;

in vec2 uv;

uniform ivec2 cell = ivec2(8);
uniform ivec2 char_pos = ivec2(0);

out vec4 color;

void main(void)
{
    vec2 inv_uv = vec2(uv.x, 1 - uv.y);
    ivec2 pos = ivec2(floor(inv_uv * cell));
    color = texelFetch(tex_font, char_pos + pos, 0);
    if (color.rgb == vec3(0.0f))
      discard;
}
