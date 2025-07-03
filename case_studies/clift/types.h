#ifndef TYPES_H
#define TYPES_H
#include <stdlib.h>
typedef struct {
  int embedding_dim;  // Token representation (embedding) dimension
  int hidden_dim;     // Intermediate representation dimension in the FFN
  int layer_count;    // Number of decoder layers
  int q_head_count;   // Number of query heads
  int kv_head_count;  // Number of key/value heads
  int vocabulary_len; // Vocabulary size
  int context_len;    // Maximum sequence length
} configuration_t;

typedef struct {
  // Embedding parameter set
  float *embedding_weight; // [vocabulary_len][embedding_dim]
  // Decoder parameter set
  // - Multi-head attention
  float *mha_norm_weight; // [layer_count][embedding_dim]
  float *mha_q_weight;    // (layer, dim, q_head_count * head_dim)
  float *mha_k_weight;    // (layer, dim, kv_head_count * head_dim)
  float *mha_v_weight;    // (layer, dim, kv_head_count * head_dim)
  float *mha_out_weight;  // (layer, q_head_count * head_dim, embedding_dim)
  // - Feed-forward network
  float *ffn_norm_weight; // (layer, dim) ffn_norm_weight
  float *ffn_fc_weight;   // (layer, hidden_dim, dim)
  float *ffn_up_weight;   // (layer, hidden_dim, dim)
  float *ffn_out_weight;  // (layer, dim, hidden_dim)
  // Output parameter set
  float *out_norm_weight; // (dim,)
  float *out_weight;      // (vocabulary_len, dim)
} parameter_set_t;

typedef struct {
  // Activations
  float *embedding;
  float *mha_norm;
  float *mha_q;
  float *mha_k_act;
  float *mha_v_act;
  float *mha_score; // buffer for scores/attention values (q_head_count,
                    // context_len)
  float *mha_blend;
  float *mha_att;
  float *mha_out;
  float *ffn_norm;
  float *ffn_fc;
  float *ffn_up;
  float *ffn_out;
  float *logits; // output logits
  // KV-cache
  float *k_cache; // (layer, context_len, dim)
  float *v_cache; // (layer, context_len, dim)
} state_t;

typedef struct {
  configuration_t config; // Hyperparameters
  parameter_set_t params; // Weights
  state_t state;          // Activations
  int fd;                 // file descriptor for memory mapping
  float *data;            // memory mapped data pointer
  ssize_t file_size;      // size of the checkpoint file in bytes
} transformer_t;
#endif
