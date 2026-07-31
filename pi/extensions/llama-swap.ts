import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

export default function (pi: ExtensionAPI) {
  pi.registerProvider("llama-swap", {
    name: "Llama Swap",
    baseUrl: "http://turkishDelight:8080/v1",
    apiKey: "$LLAMA_SWAP_API_KEY",
    api: "openai-completions",
    models: [
      {
        id: "GLM-4.5-Air-Q6_K",
        name: "GLM-4.5-Air-Q6_K",
        reasoning: false,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "GLM-4.7-Flash-THEROCK",
        name: "GLM-4.7-Flash-THEROCK",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131070,
        maxTokens: 8192
      },
      {
        id: "Qwen3.5-35B-A3B-Claude-4.6-Opus-Reasoning-Distilled-Q8_0",
        name: "Qwen3.5-35B-A3B-Claude-4.6-Opus",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "RNJ-1-Instruct",
        name: "RNJ-1-Instruct",
        reasoning: false,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 262144,
        maxTokens: 8192
      },
      {
        id: "Gemma4-Coding-Q8_0",
        name: "Gemma4-Coding-Q8_0",
        reasoning: false,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "Qwable-27B-Q4_K_M",
        name: "Qwable-27B-Q4_K_M",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "Gemma-4-12B-IT-BF16",
        name: "Gemma-4-12B-IT-BF16",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "Nex-AGI-N2-Pro-IQ1_M",
        name: "Nex-AGI-N2-Pro-IQ1_M",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "Qwen3-Coder-Next-UD-Q8",
        name: "Qwen3-Coder-Next-UD-Q8",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "Qwen3.6-35B-A3B-MTP-UD-Q8",
        name: "Qwen3.6-35B-A3B-MTP",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      },
      {
        id: "Gemma-4-26B-A4B-UD-Q8",
        name: "Gemma-4-26B-A4B",
        reasoning: true,
        input: ["text"],
        cost: { input: 0, output: 0, cacheRead: 0, cacheWrite: 0 },
        contextWindow: 131072,
        maxTokens: 8192
      }
    ]
  });
}
