#version 430 core

layout (location = 0) in vec3 position;

uniform mat4 mv = mat4(1.0);

void main(void)
{
    gl_Position = mv * vec4(position, 1.0);
}
