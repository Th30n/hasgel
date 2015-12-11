#version 330 core

layout (triangles) in;
layout (line_strip, max_vertices = 8) out;

uniform mat4 proj = mat4(1.0);
uniform mat4 model = mat4(1.0);

uniform vec4 normal_color = vec4(0.0, 1.0, 0.0, 1.0);
uniform float normal_length = 1.0;
uniform bool draw_face_normal = false;

in VS_OUT
{
    vec3 position; // Non transformed position.
    vec3 normal; // Non transformed normal.
    vec2 tc;
} gs_in[];

out vec4 color;

vec3 normal_end(vec3 pos, vec3 normal)
{
    return pos + normal * normal_length;
}

void emit_face_normal(void)
{
    mat4 mvp = proj * model;
    vec3 ab = gs_in[1].position - gs_in[0].position;
    vec3 ac = gs_in[2].position - gs_in[0].position;
    vec3 face_normal = normalize(cross(ab, ac));
    vec3 tri_centroid = (gs_in[0].position +
                         gs_in[1].position +
                         gs_in[2].position) / 3.0;
    gl_Position = mvp * vec4(tri_centroid, 1.0);
    color = normal_color;
    EmitVertex();
    
    vec3 end = normal_end(tri_centroid, face_normal);
    gl_Position = mvp * vec4(end, 1.0);
    color = normal_color;
    EmitVertex();
    EndPrimitive();
}

void main(void)
{
    mat4 mvp = proj * model;
    for (int i = 0; i < 3; ++i) {
        gl_Position = gl_in[i].gl_Position;
        color = normal_color;
        EmitVertex();

        vec3 end = normal_end(gs_in[i].position, gs_in[i].normal);
        gl_Position = mvp * vec4(end, 1.0);
        color = normal_color;
        EmitVertex();
        EndPrimitive();
    }
    if (draw_face_normal)
        emit_face_normal();
}