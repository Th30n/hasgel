#version 430 core

layout (location = 0) in vec3 position;

uniform mat4 proj = mat4(1.0);
uniform mat4 model = mat4(1.0);

out VS_OUT
{
    vec3 position;
    vec2 tc;
} vs_out;

void main(void)
{
    gl_Position = proj * model * vec4(position, 1.0);
    vs_out.tc = position.xy;
    vs_out.position = position;
}
