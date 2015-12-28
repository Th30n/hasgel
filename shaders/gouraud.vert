#version 430 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 mvp = mat4(1.0f);
uniform mat3 normal_model = mat3(1.0f);

// Directional light
uniform vec3 light_color = vec3(1.0f);
uniform vec3 light_direction = vec3(1.0f, 0.0f, 0.5f);

// Material
uniform vec3 diff_color = vec3(1.0f);

out VS_OUT
{
    vec2 uv;
    vec4 color;
} vs_out;

void main()
{
    gl_Position = mvp * vec4(position, 1.0f);
    // Lighting in world space.
    vec3 n = normalize(normal_model * normal);
    float ndotl = max(0.0f, dot(n, light_direction));
    vs_out.uv = uv;
    vs_out.color = vec4(diff_color * light_color * ndotl, 1.0);
}
