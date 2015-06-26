#version 430 core

out VS_OUT
{
    vec4 color;
} vs_out;

void main(void)
{
    const vec4 vertices[6] = vec4[6](vec4(0.0, 0.0, 0.0, 1.0),
                                     vec4(1.0, 0.0, 0.0, 1.0),
                                     vec4(0.0, 0.0, 0.0, 1.0),
                                     vec4(0.0, 1.0, 0.0, 1.0),
                                     vec4(0.0, 0.0, 0.0, 1.0),
                                     vec4(0.0, 0.0, 1.0, 1.0));
    gl_Position = vertices[gl_VertexID];
    vs_out.color = vertices[(gl_VertexID / 2) * 2 + 1];
}
