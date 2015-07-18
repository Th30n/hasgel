#version 330 core

layout (points) in;
layout (line_strip) out;
layout (max_vertices = 6) out;

out vec4 color;

void main(void)
{
    const vec4 axis[3] = vec4[3](vec4(1.0, 0.0, 0.0, 1.0),
                                 vec4(0.0, 1.0, 0.0, 1.0),
                                 vec4(0.0, 0.0, 1.0, 1.0));
    for (int i = 0; i < 3; ++i) {
        gl_Position = gl_in[0].gl_Position;
        color = axis[i];
        EmitVertex();
        gl_Position = axis[i];
        color = axis[i];
        EmitVertex();
        EndPrimitive();
    }
}