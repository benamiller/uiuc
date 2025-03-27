# Reproducing Time-Aware Transformer-based Network for Clinical Notes Series Prediction

## Introduction

### Problem Statement
This paper addresses the critical challenge of predicting future clinical notes based on a patient's historical medical records. Clinical note prediction is essential for healthcare providers as it can help anticipate patient conditions, streamline documentation processes, and improve care continuity. The specific problem tackled is how to effectively model both the semantic content and temporal relationships in sequential clinical notes while handling the inherent complexities of medical text data, including variable time intervals between notes and complex medical terminology.

### Citation
Zhang, D., Thadajarassiri, J., Sen, C., & Rundensteiner, E. (2020). Time-Aware Transformer-Based Network for Clinical Notes Series Prediction. Proceedings of Machine Learning for Healthcare, PMLR 126:566-588.

## Methodology

### Specific Approach
The authors propose a Time-Aware Transformer-based Network (TaT-Net) that extends the traditional Transformer architecture to incorporate temporal information in clinical note sequences. The approach consists of several key components:

1. Time-Aware Attention Mechanism:
   - Modifies the standard self-attention by incorporating time interval information
   - Uses temporal embedding to capture time gaps between notes
   - Implements a novel time-aware positional encoding

2. Hierarchical Architecture:
   - Word-level encoder to capture semantic meanings
   - Note-level encoder to model note-to-note relationships
   - Temporal attention layer to handle irregular time intervals

The model is evaluated using standard metrics including:
- ROUGE-1, ROUGE-2, and ROUGE-L scores for text generation quality
- Perplexity for measuring prediction confidence
- BLEU score for assessing translation quality
- Medical concept accuracy using UMLS mapping

### Novelty and Relevance
The primary novelty lies in the integration of temporal information into the Transformer architecture specifically for clinical text. Unlike previous approaches that either ignored temporal aspects or treated time as a simple sequential feature, TaT-Net explicitly models time intervals between notes. The authors hypothesize that incorporating precise temporal information will improve the quality of predicted clinical notes compared to time-agnostic models.

The method improves upon baselines by:
1. Explicitly modeling irregular time intervals between notes
2. Maintaining long-term dependencies through the modified attention mechanism
3. Preserving medical semantic accuracy through the hierarchical structure

### Planned Ablations and Extensions
I plan to investigate several extensions to validate and potentially improve the model:

1. Alternative Temporal Embeddings:
   - Experiment with different temporal encoding schemes
   - Investigate the impact of time granularity on prediction quality

2. Medical Knowledge Integration:
   - Incorporate pre-trained clinical BERT embeddings
   - Add UMLS concept regularization

3. Architectural Modifications:
   - Test different attention mechanisms
   - Explore the impact of varying encoder depths

## Data Access and Implementation Details

### Dataset and Code Availability
The paper uses the MIMIC-III database, which is publicly available through PhysioNet. I have already completed the required CITI training and obtained access credentials. The specific dataset consists of clinical notes from the MIMIC-III database, focusing on discharge summaries and progress notes.

The authors have not released their original implementation. However, the paper provides detailed architectural specifications and hyperparameters that make reproduction feasible.

### Computational Feasibility
The implementation will be done using Google Colab with T4 GPU support. Based on the model architecture and dataset size described in the paper:

- Expected training time: 4-6 hours per epoch on Colab T4 GPU
- Memory requirements: ~12GB GPU memory (within Colab's 16GB limit)
- Dataset processing: Requires ~30GB storage (manageable with mounted Google Drive)

The computational requirements are feasible within the free tier of Google Colab, though training will need to be broken into multiple sessions due to runtime limits.

### Code Implementation Strategy
I will use the official implementation available at https://github.com/zdy93/FTL-Trans as the foundation for reproduction. This approach will:
- Allow direct verification against the original results
- Provide a validated codebase for ablation studies
- Enable focused study of the key architectural components
- Facilitate contribution to PyHealth through a verified implementation
- Ensure reproducibility through use of the original code

## References

1. Vaswani, A., et al. (2017). Attention is all you need. Advances in neural information processing systems, 30.

2. Johnson, A. E., et al. (2016). MIMIC-III, a freely accessible critical care database. Scientific data, 3(1), 1-9.

3. Devlin, J., et al. (2018). BERT: Pre-training of deep bidirectional transformers for language understanding. arXiv preprint arXiv:1810.04805.

4. Liu, Y., et al. (2019). RoBERTa: A robustly optimized BERT pretraining approach. arXiv preprint arXiv:1907.11692.

5. Lee, J., et al. (2020). BioBERT: a pre-trained biomedical language representation model for biomedical text mining. Bioinformatics, 36(4), 1234-1240.
