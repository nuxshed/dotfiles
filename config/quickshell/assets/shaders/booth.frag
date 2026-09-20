#version 440

layout(location = 0) in vec2 qt_TexCoord0;
layout(location = 0) out vec4 fragColor;

layout(std140, binding = 0) uniform buf {
    mat4 qt_Matrix;
    float qt_Opacity;
    float aspect;
    float mirror;
    int effect;
    vec2 texel;
};

layout(binding = 1) uniform sampler2D source;

const float PI = 3.14159265;

float luma(vec3 c) {
    return dot(c, vec3(0.299, 0.587, 0.114));
}

vec2 warp(vec2 uv, int e) {
    vec2 p = (uv - 0.5) * vec2(aspect, 1.0);
    float r = length(p);
    float a = atan(p.y, p.x);
    if (e == 8) {
        r = pow(r / 0.7, 1.6) * 0.7;
    } else if (e == 9) {
        r = pow(r / 0.7, 0.6) * 0.7;
    } else if (e == 10) {
        float s = clamp(1.0 - r / 0.6, 0.0, 1.0);
        a += s * s * 4.0;
    } else if (e == 11) {
        p.x = sign(p.x) * pow(abs(p.x) / 0.9, 1.8) * 0.9;
        return p / vec2(aspect, 1.0) + 0.5;
    } else if (e == 12) {
        p.x = -abs(p.x);
        return p / vec2(aspect, 1.0) + 0.5;
    } else if (e == 13) {
        r = r * (1.0 + 1.4 * r * r) * 0.55;
    } else if (e == 14) {
        p.y = sign(p.y) * pow(abs(p.y) / 0.5, 0.5) * 0.5;
        return p / vec2(aspect, 1.0) + 0.5;
    } else if (e == 15) {
        r = min(r, 0.38);
    } else if (e == 7) {
        float seg = PI / 3.0;
        a = mod(a, seg);
        a = abs(a - seg * 0.5);
        r *= 0.8;
    } else {
        return uv;
    }
    p = vec2(cos(a), sin(a)) * r;
    return p / vec2(aspect, 1.0) + 0.5;
}

vec3 thermal(float t) {
    vec3 c = mix(vec3(0.05, 0.02, 0.35), vec3(0.0, 0.55, 0.85), smoothstep(0.0, 0.25, t));
    c = mix(c, vec3(0.1, 0.8, 0.2), smoothstep(0.25, 0.5, t));
    c = mix(c, vec3(1.0, 0.9, 0.1), smoothstep(0.5, 0.75, t));
    c = mix(c, vec3(1.0, 0.2, 0.05), smoothstep(0.75, 0.9, t));
    c = mix(c, vec3(1.0), smoothstep(0.9, 1.0, t));
    return c;
}

void main() {
    vec2 uv = qt_TexCoord0;
    if (mirror > 0.5)
        uv.x = 1.0 - uv.x;

    if (effect == 5) {
        vec2 cell = floor(uv * 2.0);
        vec2 sub = fract(uv * 2.0);
        float t = luma(texture(source, sub).rgb);
        t = floor(t * 3.0) / 3.0;
        int k = int(cell.x + cell.y * 2.0);
        vec3 lo = k == 0 ? vec3(0.9, 0.1, 0.4) : k == 1 ? vec3(0.1, 0.3, 0.9) : k == 2 ? vec3(0.1, 0.7, 0.3) : vec3(0.95, 0.5, 0.05);
        vec3 hi = k == 0 ? vec3(1.0, 0.95, 0.2) : k == 1 ? vec3(1.0, 0.4, 0.8) : k == 2 ? vec3(1.0, 0.9, 0.3) : vec3(0.3, 0.1, 0.6);
        fragColor = vec4(mix(lo, hi, t), 1.0) * qt_Opacity;
        return;
    }

    vec2 w = warp(uv, effect);
    if (effect >= 8 && effect != 12 && (w.x < 0.0 || w.x > 1.0 || w.y < 0.0 || w.y > 1.0)) {
        fragColor = vec4(0.0, 0.0, 0.0, qt_Opacity);
        return;
    }
    vec3 c = texture(source, w).rgb;

    if (effect == 1) {
        float t = luma(c);
        c = vec3(t * 1.15, t * 0.95, t * 0.7);
    } else if (effect == 2) {
        c = vec3(luma(c));
    } else if (effect == 3) {
        c = thermal(luma(c));
    } else if (effect == 4) {
        float t = 1.0 - luma(c);
        c = vec3(t * 0.8, t * 0.95, t * 1.1);
    } else if (effect == 6) {
        vec2 px = texel * 2.5;
        float gx = luma(texture(source, w + vec2(px.x, 0.0)).rgb) - luma(texture(source, w - vec2(px.x, 0.0)).rgb);
        float gy = luma(texture(source, w + vec2(0.0, px.y)).rgb) - luma(texture(source, w - vec2(0.0, px.y)).rgb);
        float edge = smoothstep(0.18, 0.45, length(vec2(gx, gy)));
        c = floor(c * 4.0 + 0.5) / 4.0;
        c = mix(c * 1.15, vec3(0.05), edge);
    }

    fragColor = vec4(clamp(c, 0.0, 1.0), 1.0) * qt_Opacity;
}
