#version 430 core

layout (points) in;
layout (triangle_strip, max_vertices = 4) out;

uniform mat4 proj = mat4(1.0);
// Half the width of a quad.
uniform float size = 1.0f;

out vec2 uv;

void main(void)
{
    vec4 pos = gl_in[0].gl_Position;

    vec4 bot_left = vec4(-size, -size, 0.0f, 0.0f) + pos;
    gl_Position = proj * bot_left;
    uv = vec2(0.0f, 0.0f);
    EmitVertex();

    vec4 bot_right = vec4(size, -size, 0.0f, 0.0f) + pos;
    gl_Position = proj * bot_right;
    uv = vec2(1.0f, 0.0f);
    EmitVertex();

    vec4 top_left = vec4(-size, size, 0.0f, 0.0f) + pos;
    gl_Position = proj * top_left;
    uv = vec2(0.0f, 1.0f);
    EmitVertex();

    vec4 top_right = vec4(size, size, 0.0f, 0.0f) + pos;
    gl_Position = proj * top_right;
    uv = vec2(1.0f, 1.0f);
    EmitVertex();

    EndPrimitive();
}
