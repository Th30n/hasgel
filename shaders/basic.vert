#version 430 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

uniform mat4 mvp = mat4(1.0);

out VS_OUT
{
    vec3 position;
    vec3 normal;
    vec2 tc;
} vs_out;

void main(void)
{
    gl_Position = mvp * vec4(position, 1.0);
    vs_out.tc = position.xy - 1E-5;
    vs_out.position = position;
    vs_out.normal = normal;
}
