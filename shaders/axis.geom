#version 330 core

layout (points) in;
layout (line_strip) out;
layout (max_vertices = 6) out;

uniform mat4 rotation = mat4(1.0f);

out vec4 color;

void main(void)
{
    const vec4 axis[3] = vec4[3](vec4(1.0, 0.0, 0.0, 0.0),
                                 vec4(0.0, 1.0, 0.0, 0.0),
                                 vec4(0.0, 0.0, 1.0, 0.0));
    for (int i = 0; i < 3; ++i) {
        gl_Position = gl_in[0].gl_Position;
        color = axis[i];
        EmitVertex();
        vec4 dir = rotation * axis[i];
        gl_Position = gl_in[0].gl_Position + 0.5 * dir;
        color = vec4(axis[i].rgb, 1.0f);
        EmitVertex();
        EndPrimitive();
    }
}