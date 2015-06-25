#version 430 core

layout (location = 0) in vec4 offset;

out VS_OUT
{
    vec2 tc;
} vs_out;

void main(void)
{
    const vec4 vertices[3] = vec4[3](vec4(0.25, -0.25, 0.5, 1.0),
                                     vec4(-0.25, -0.25, 0.5, 1.0),
                                     vec4(0.25, 0.25, 0.5, 1.0));
    gl_Position = vertices[gl_VertexID] + offset;
    vs_out.tc = gl_Position.xy;
}
