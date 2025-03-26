from collections import defaultdict

# Step 1: Read the input file
with open("joSxjoQCToy8PKM-BN6GEg_068e350481c34ee7ab9b567ea9e66af1_reviews_sample.txt", "r") as f:
    reviews = [line.strip().split() for line in f]

# Step 2: Find frequent 1-sequences (words with support >= 100)
word_to_reviews = defaultdict(set)
for review_idx, review in enumerate(reviews):
    # Use set to count each word once per review
    unique_words = set(review)
    for word in unique_words:
        word_to_reviews[word].add(review_idx)

min_support = 100
F1 = {word for word in word_to_reviews if len(word_to_reviews[word]) >= min_support}

# Initialize result dictionary with frequent 1-sequences
# Store as single-item tuples for consistency
result = {(word,): len(word_to_reviews[word]) for word in F1}

# Prepare F_prev as a set of tuples for the iterative step
F_prev = {(word,) for word in F1}

# Step 3: Mine frequent k-sequences (k >= 2)
k = 2
while True:
    # Dictionary to store candidate k-sequences and the reviews they appear in
    candidate_to_reviews = defaultdict(set)
    
    # Generate candidates from each review
    for review_idx, review in enumerate(reviews):
        n = len(review)
        # For each possible starting position of a k-length sequence
        for i in range(n - k + 1):
            # Extract prefix (first k-1 items) and suffix (last k-1 items)
            prefix = tuple(review[i:i + k - 1])
            suffix = tuple(review[i + 1:i + k])
            # Check if both prefix and suffix are frequent (k-1)-sequences
            if prefix in F_prev and suffix in F_prev:
                seq = tuple(review[i:i + k])
                candidate_to_reviews[seq].add(review_idx)
    
    # Filter candidates with support >= min_support
    F_k = {seq for seq in candidate_to_reviews if len(candidate_to_reviews[seq]) >= min_support}
    
    # If no frequent sequences are found, stop
    if not F_k:
        break
    
    # Add frequent k-sequences to result
    for seq in F_k:
        support = len(candidate_to_reviews[seq])
        result[seq] = support
    
    # Update F_prev for the next iteration
    F_prev = F_k
    k += 1

# Step 4: Write results to output file
with open("patterns.txt", "w") as f:
    for seq, support in result.items():
        # Format the sequence with semicolons and spaces
        pattern_str = ';'.join(seq)
        # Write in the format "support: pattern"
        f.write(f"{support}:{pattern_str}\n")
