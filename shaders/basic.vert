#version 430 core

layout (location = 0) in vec4 position;
layout (location = 1) in vec4 offset;

out VS_OUT
{
    vec2 tc;
} vs_out;

void main(void)
{
    gl_Position = position + offset;
    vs_out.tc = gl_Position.xy;
}
