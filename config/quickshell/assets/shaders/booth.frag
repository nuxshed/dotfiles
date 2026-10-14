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
    float time;
};

layout(binding = 1) uniform sampler2D source;
layout(binding = 2) uniform sampler2D fast;
layout(binding = 3) uniform sampler2D slow;

const float PI = 3.14159265;

const int NORMAL = 0;
const int MONO = 1;
const int THERMAL = 2;
const int XRAY = 3;
const int POPART = 4;
const int COMIC = 5;
const int GLOW = 6;
const int BULGE = 7;
const int TWIRL = 8;
const int SQUEEZE = 9;
const int FISHEYE = 10;
const int LMIRROR = 11;
const int RMIRROR = 12;
const int TMIRROR = 13;
const int BMIRROR = 14;
const int FLIP = 15;
const int VINTAGE = 16;
const int RETRO = 17;
const int ROSE = 18;
const int DUOTONE = 19;
const int NIGHT = 20;
const int GHOST = 21;
const int CGHOST = 22;
const int GLITCH = 23;
const int VHS = 24;
const int CRT = 25;
const int BIT8 = 26;
const int HALFTONE = 27;
const int HATCH = 28;
const int EMBOSS = 29;
const int CYBER = 30;
const int PRISM = 31;

float luma(vec3 c) {
    return dot(c, vec3(0.299, 0.587, 0.114));
}

float hash(vec2 p) {
    return fract(sin(dot(p, vec2(12.9898, 78.233))) * 43758.5453);
}

vec3 tap(vec2 uv) {
    return texture(source, clamp(uv, 0.0, 1.0)).rgb;
}

float edge(vec2 uv, float spread) {
    vec2 px = texel * spread;
    float gx = luma(tap(uv + vec2(px.x, 0.0))) - luma(tap(uv - vec2(px.x, 0.0)));
    float gy = luma(tap(uv + vec2(0.0, px.y))) - luma(tap(uv - vec2(0.0, px.y)));
    return length(vec2(gx, gy));
}

float vignette(vec2 uv, float strength) {
    vec2 p = (uv - 0.5) * vec2(aspect, 1.0);
    return 1.0 - strength * smoothstep(0.25, 0.85, length(p));
}

vec2 fromPolar(float r, float a) {
    return vec2(cos(a), sin(a)) * r / vec2(aspect, 1.0) + 0.5;
}

vec2 warp(vec2 uv) {
    vec2 p = (uv - 0.5) * vec2(aspect, 1.0);
    float r = length(p);
    float a = atan(p.y, p.x);
    if (effect == BULGE)
        return fromPolar(pow(r / 0.7, 1.6) * 0.7, a);
    if (effect == TWIRL) {
        float s = clamp(1.0 - r / 0.6, 0.0, 1.0);
        return fromPolar(r, a + s * s * 4.0);
    }
    if (effect == SQUEEZE) {
        p.x = sign(p.x) * pow(abs(p.x) / 0.9, 1.8) * 0.9;
        return p / vec2(aspect, 1.0) + 0.5;
    }
    if (effect == LMIRROR || effect == RMIRROR) {
        p.x = effect == LMIRROR ? -abs(p.x) : abs(p.x);
        return p / vec2(aspect, 1.0) + 0.5;
    }
    if (effect == TMIRROR || effect == BMIRROR) {
        p.y = effect == TMIRROR ? -abs(p.y) : abs(p.y);
        return p / vec2(aspect, 1.0) + 0.5;
    }
    if (effect == FLIP)
        return vec2(uv.x, 1.0 - uv.y);
    if (effect == FISHEYE)
        return fromPolar(r * (1.0 + 1.4 * r * r) * 0.55, a);
    if (effect == CRT)
        return p * (1.0 + 0.18 * r * r) / vec2(aspect, 1.0) + 0.5;
    if (effect == BIT8) {
        vec2 cells = vec2(110.0 * aspect, 110.0);
        return (floor(uv * cells) + 0.5) / cells;
    }
    return uv;
}

vec3 thermal(float t) {
    vec3 c = mix(vec3(0.05, 0.02, 0.35), vec3(0.0, 0.55, 0.85), smoothstep(0.0, 0.25, t));
    c = mix(c, vec3(0.1, 0.8, 0.2), smoothstep(0.25, 0.5, t));
    c = mix(c, vec3(1.0, 0.9, 0.1), smoothstep(0.5, 0.75, t));
    c = mix(c, vec3(1.0, 0.2, 0.05), smoothstep(0.75, 0.9, t));
    c = mix(c, vec3(1.0), smoothstep(0.9, 1.0, t));
    return c;
}

vec3 cyber(float t) {
    vec3 c = mix(vec3(0.04, 0.02, 0.12), vec3(0.45, 0.05, 0.55), smoothstep(0.1, 0.35, t));
    c = mix(c, vec3(1.0, 0.15, 0.55), smoothstep(0.35, 0.55, t));
    c = mix(c, vec3(0.1, 0.85, 1.0), smoothstep(0.55, 0.78, t));
    c = mix(c, vec3(1.0, 0.95, 0.6), smoothstep(0.78, 0.95, t));
    return c;
}

vec3 glow(vec2 uv, vec3 c) {
    vec3 blur = vec3(0.0);
    for (int i = 0; i < 12; i++) {
        float a = float(i) * PI / 6.0;
        blur += tap(uv + vec2(cos(a), sin(a)) * texel * 7.0);
        blur += tap(uv + vec2(cos(a + 0.26), sin(a + 0.26)) * texel * 14.0);
    }
    blur /= 24.0;
    vec3 bright = max(blur - 0.45, 0.0) * 1.8;
    return mix(c, blur, 0.25) + bright;
}

void main() {
    vec2 uv = qt_TexCoord0;

    if (mirror > 0.5)
        uv.x = 1.0 - uv.x;

    if (effect == POPART) {
        vec2 cell = floor(uv * 2.0);
        vec2 sub = fract(uv * 2.0);
        float t = floor(luma(texture(source, sub).rgb) * 3.0) / 3.0;
        float k = cell.x + cell.y * 2.0;
        vec3 lo = k < 0.5 ? vec3(0.9, 0.1, 0.4) : k < 1.5 ? vec3(0.1, 0.3, 0.9) : k < 2.5 ? vec3(0.1, 0.7, 0.3) : vec3(0.95, 0.5, 0.05);
        vec3 hi = k < 0.5 ? vec3(1.0, 0.95, 0.2) : k < 1.5 ? vec3(1.0, 0.4, 0.8) : k < 2.5 ? vec3(1.0, 0.9, 0.3) : vec3(0.3, 0.1, 0.6);
        fragColor = vec4(mix(lo, hi, t), 1.0) * qt_Opacity;
        return;
    }

    vec2 w = warp(uv);
    bool outside = w.x < 0.0 || w.x > 1.0 || w.y < 0.0 || w.y > 1.0;
    if (outside && effect != LMIRROR && effect != RMIRROR && effect != TMIRROR && effect != BMIRROR) {
        fragColor = vec4(0.0, 0.0, 0.0, qt_Opacity);
        return;
    }
    vec3 c = tap(w);
    float grain = hash(uv * 512.0 + fract(time * 7.0)) - 0.5;

    if (effect == MONO) {
        c = vec3(luma(c));
    } else if (effect == VINTAGE) {
        vec3 soft = (c * 2.0 + tap(w + texel * vec2(1.5, 0.0)) + tap(w - texel * vec2(1.5, 0.0)) + tap(w + texel * vec2(0.0, 1.5)) + tap(w - texel * vec2(0.0, 1.5))) / 6.0;
        float t = 0.3 + luma(soft) * 0.72;
        vec3 tone = mix(vec3(0.44, 0.29, 0.31), vec3(1.0, 0.9, 0.8), smoothstep(0.2, 1.0, t));
        c = mix(tone, soft * tone * 1.25, 0.12);
        vec2 p = (uv - 0.5) * vec2(aspect, 1.0);
        c = mix(c, vec3(1.0, 0.93, 0.85), smoothstep(0.45, 1.0, length(p)) * 0.45);
        c += grain * 0.07;
    } else if (effect == RETRO) {
        float t = luma(c);
        c = mix(c, vec3(t), 0.2) * 0.86 + vec3(0.05, 0.06, 0.07);
        c *= mix(vec3(0.82, 1.0, 1.06), vec3(1.14, 0.98, 0.8), smoothstep(0.1, 0.9, t));
        c = c * vignette(uv, 0.4) + grain * 0.05;
    } else if (effect == ROSE) {
        float t = luma(c);
        c = mix(c, vec3(t) * vec3(1.05, 0.74, 0.8), 0.55) * 1.06 + vec3(0.06, 0.01, 0.03);
    } else if (effect == GHOST) {
        vec3 trail = texture(slow, w).rgb;
        c = mix(trail, c, 0.32);
        c = mix(c, vec3(luma(c)) * vec3(0.86, 0.96, 1.1), 0.45) * 1.08;
    } else if (effect == CGHOST) {
        c = vec3(c.r, texture(fast, w).g, texture(slow, w).b);
    } else if (effect == DUOTONE) {
        c = mix(vec3(0.16, 0.05, 0.36), vec3(1.0, 0.62, 0.32), smoothstep(0.08, 0.92, luma(c)));
    } else if (effect == GLOW) {
        c = glow(w, c);
    } else if (effect == THERMAL) {
        c = thermal(luma(c));
    } else if (effect == XRAY) {
        float t = 1.0 - luma(c);
        c = vec3(t * 0.8, t * 0.95, t * 1.1);
    } else if (effect == NIGHT) {
        float t = luma(c) * 1.6 + grain * 0.18;
        float scan = 0.88 + 0.12 * sin(uv.y / texel.y * 1.6);
        vec2 p = (uv - 0.5) * vec2(aspect, 1.0);
        float lens = 1.0 - smoothstep(0.42, 0.5, length(p));
        c = vec3(0.12, 1.0, 0.25) * t * scan * lens;
    } else if (effect == COMIC) {
        float e = smoothstep(0.18, 0.45, edge(w, 2.5));
        c = floor(c * 4.0 + 0.5) / 4.0;
        c = mix(c * 1.15, vec3(0.05), e);
    } else if (effect == GLITCH) {
        float tick = floor(time * 9.0);
        float band = floor(uv.y * 28.0);
        float n = hash(vec2(band, tick));
        vec2 g = w;
        if (n > 0.82)
            g.x += (hash(vec2(tick, band + 7.0)) - 0.5) * 0.12;
        float split = 0.004 + 0.012 * step(0.9, hash(vec2(tick, 3.0)));
        c = vec3(tap(g + vec2(split, 0.0)).r, tap(g).g, tap(g - vec2(split, 0.0)).b);
        if (hash(vec2(band, tick + 1.0)) > 0.96)
            c = 1.0 - c;
    } else if (effect == VHS) {
        float roll = fract(time * 0.12);
        float track = smoothstep(0.03, 0.0, abs(uv.y - roll));
        vec2 g = w + vec2(sin(uv.y * 180.0 + time * 12.0) * 0.0009 + track * 0.02 * (hash(vec2(uv.y, time)) - 0.5), 0.0);
        c = vec3(tap(g + vec2(0.003, 0.0)).r, tap(g).g, tap(g - vec2(0.003, 0.0)).b);
        c = mix(c, vec3(luma(c)), 0.25) * vec3(1.05, 0.98, 1.08);
        c *= 0.9 + 0.1 * sin(uv.y / texel.y * 1.2);
        c += track * 0.35 * hash(uv * 300.0 + time) + grain * 0.08;
    } else if (effect == CRT) {
        float scan = 0.78 + 0.22 * sin(w.y / texel.y * 2.1);
        float m = mod(floor(gl_FragCoord.x), 3.0);
        vec3 mask = m < 0.5 ? vec3(1.0, 0.75, 0.75) : m < 1.5 ? vec3(0.75, 1.0, 0.75) : vec3(0.75, 0.75, 1.0);
        c = c * scan * mask * 1.25 * vignette(w, 0.5);
    } else if (effect == BIT8) {
        c = floor(c * 4.0 + 0.5) / 4.0;
    } else if (effect == HALFTONE) {
        float size = 1.0 / 70.0;
        vec2 p = uv * vec2(aspect, 1.0);
        mat2 rot = mat2(0.7071, -0.7071, 0.7071, 0.7071);
        vec2 q = rot * p / size;
        vec2 center = (floor(q) + 0.5);
        mat2 back = mat2(0.7071, 0.7071, -0.7071, 0.7071);
        vec2 cuv = (back * (center * size)) / vec2(aspect, 1.0);
        vec3 cc = tap(cuv);
        float radius = sqrt(1.0 - luma(cc)) * 0.62;
        float d = length(q - center);
        c = mix(vec3(0.98, 0.96, 0.9), cc * 0.85, 1.0 - smoothstep(radius - 0.08, radius + 0.08, d));
    } else if (effect == HATCH) {
        float t = luma(c);
        vec2 f = gl_FragCoord.xy;
        float ink = 0.0;
        if (t < 0.62 && mod(f.x + f.y, 8.0) < 1.0)
            ink = 1.0;
        if (t < 0.44 && mod(f.x - f.y, 8.0) < 1.0)
            ink = 1.0;
        if (t < 0.28 && mod(f.x + f.y - 4.0, 8.0) < 1.0)
            ink = 1.0;
        if (t < 0.15 && mod(f.x - f.y - 4.0, 8.0) < 1.0)
            ink = 1.0;
        float e = smoothstep(0.05, 0.2, edge(w, 1.5));
        c = mix(vec3(0.97, 0.95, 0.9), vec3(0.12, 0.12, 0.18), max(ink, e));
    } else if (effect == EMBOSS) {
        vec2 px = texel * 2.0;
        float d = luma(tap(w - px)) - luma(tap(w + px));
        c = vec3(0.5 + d * 2.5) * vec3(1.0, 0.98, 0.95);
    } else if (effect == CYBER) {
        float e = smoothstep(0.1, 0.35, edge(w, 1.5));
        c = cyber(luma(c)) + vec3(0.1, 0.9, 1.0) * e * 0.6;
    } else if (effect == PRISM) {
        vec2 o = vec2(0.035, 0.012);
        c = vec3(tap(w + o).r, tap(w).g, tap(w - o).b) * 0.7 + tap(w + o * 2.2) * vec3(0.3, 0.05, 0.25) + tap(w - o * 2.2) * vec3(0.05, 0.25, 0.3);
    }

    fragColor = vec4(clamp(c, 0.0, 1.0), 1.0) * qt_Opacity;
}
