#version 430 core

layout (triangles) in;
layout (triangle_strip, max_vertices = 3) out;

uniform mat4 mvp;
uniform float explode_factor = 0.0f;

in GS_IN
{
    vec3 position; // Non transformed position.
    vec3 normal; // Non transformed normal.
} gs_in[];

in VS_OUT
{
    // Used in fragment shader.
    vec2 uv;
    vec3 diff;
    vec3 spec;
} vs_out[];

out VS_OUT
{
    // Used in fragment shader.
    vec2 uv;
    vec3 diff;
    vec3 spec;
} gs_out;

vec4 normal_end(vec3 pos, vec3 normal)
{
    return vec4(pos + normal * explode_factor, 1.0f);
}

void main(void)
{
    vec3 ab = gs_in[1].position - gs_in[0].position;
    vec3 ac = gs_in[2].position - gs_in[0].position;
    vec3 normal = normalize(cross(ab, ac));
    for (int i = 0; i < 3; i++) {
        gl_Position = mvp * normal_end(gs_in[i].position, normal);
        gs_out.uv = vs_out[i].uv;
        gs_out.diff = vs_out[i].diff;
        gs_out.spec = vs_out[i].spec;
        EmitVertex();
    }
    EndPrimitive();
}
