#version 430 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

// Transforms.
uniform mat4 mvp = mat4(1.0f); // Model-view-projection for position.
uniform mat3 normal_model = mat3(1.0f); // Model-view for normals.
uniform mat4 mv = mat4(1.0f); // Model-view for position.

struct light_t
{
    vec4 pos; // Light position, if w == 0, then direction.
    vec3 intensity; // Light intensity (0+)
};

// Directional light
uniform light_t dir_light = { vec4(1.0f, 0.0f, 0.5f, 0.0f),
                              vec3(1.0f) };

struct material_t
{
    vec3 diff; // Diffuse color (0-1).
    vec3 spec; // Specular color (0-1).
    float shine; // Surface smoothness factor (1-200).
};

uniform material_t mat = { vec3(1.0f),
                           vec3(0.0f),
                           1.0f };

out VS_OUT
{
    // Used in fragment shader.
    vec2 uv;
    vec3 diff;
    vec3 spec;
} vs_out;

out GS_IN
{
    vec3 position; // Model position.
    vec3 normal; // Model normal.
} gs_in;

void main()
{
    gl_Position = mvp * vec4(position, 1.0f);
    // Lighting in world space.
    vec3 n = normalize(normal_model * normal);
    float ndotl = max(0.0f, dot(n, dir_light.pos.xyz));
    vec3 diff = mat.diff * dir_light.intensity * ndotl;
    // Specular, uses half vector instead of reflected.
    vec3 v = normalize(-(mv * vec4(position, 1.0f)).xyz);
    vec3 h = normalize(dir_light.pos.xyz + v);
    float shine = max(0.0f, pow(dot(h, n), mat.shine));
    vec3 spec = mat.spec * shine * dir_light.intensity * ndotl;
    vs_out.uv = uv;
    vs_out.diff = diff;
    vs_out.spec = spec;
    gs_in.position = position;
    gs_in.normal = normal;
}
