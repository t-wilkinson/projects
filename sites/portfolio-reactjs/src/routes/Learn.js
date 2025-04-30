import React from 'react'
import ReactMarkdown from 'react-markdown'

export default () => (
  <div style={{ width: '100%', background: '#ccc' }}>
    <div
      style={{
        padding: 20,
        marginLeft: 'auto',
        marginRight: 'auto',
        maxWidth: 1000,
        background: 'white',
      }}
    >
      <ReactMarkdown>{`
- What is deep thinking?
    - You may have heard of the feynman technique, viewing of knowledge as a semantic tree, and chunking knowledge. These are all roughly equivalent to deep thinking (and each other). It is simply the process of truly understanding and developing working knowledge of a concept. So what does it mean in depth and why do it?
- Why deep think?
    - First a quick note on Intelligence vs. IQ. Intelligence is more or less the ability to think, solve problems, reason, creativity, intelligence, and understand. IQ is a number assigned to people describing abstract reasoning.
    - One thing I often notice about people considered highly intelligent is their deep understanding of concepts, conceptual relationships, etc. Of the collection of individuals with high IQ, only some seem to exhibit this truly deep understanding of concepts.  I think this says that high IQ does not necessarily imply high intelligence. Additionally these intelligent people seem to use methods that go by different names, but have the same meaning, which is what I want to share.
    - If you attempt to identify aspects of how smart people think and understand things, one of the most obvious is a deep understanding of concepts. They understand how a concept relates to plenty of other concepts. They abstract concepts and relate those abstractions to identify deep relations. They can greatly simplify a difficult concept. Simplifications allow one to do a sort of mental gymnastics with a concept as it reduces the space a concept takes in working memory, giving your more space for other things like solving problems, finding deep connections, explaining a concept, learn more concepts quicker, remember more, etc. Note that working memory is a powerful predictor of subsequent academic success [1]. So reducing the amount of space a concept takes in your mind ultimately increases your effective working memory, increasing your mental performance. This is powerful reasoning behind the effectivenes of simplifying concepts.
    - Learning with the purpose of teaching someone (not the act of teaching) dramatically increases learning effectiveness [2]. The act of learning knowledge to teach someone else forces you to organize knowledge and simlify it. The simpler and more organized a concept, the easier it will stick in your head.
    - Another argument for deep thinking is the feeling of mastery of a concept. You see the concept everywhere and constantly are developing connections. The concept feels very light in your mind and it becomes much easier to learn more difficult concepts that build off that one.
    - It obiously takes more time to deep think. But it is generally worth it. Deep thinking helps you remember a concept longer because you understand it deeply. The concept sticks with you and it becomes very quick to recall and review. Learning similar concepts are easier as you can simply learn the differences between the concept you already understand and the one you are learning. If, however, you only temporarily need a concept or you are very limited in time, it makes sense to shallow learn the concept. Also if you simply don't require deep interworking knowledge of concepts (think rote memory like learning a language) then deep thinking won't help too much.
- How to deep think?
    - There are two main aspects of deep thinking, one has to do with understanding individual concepts, the other with relating concepts you understand. Both are critical.
    - Undstanding an individual concept
        - Isolate
            - Isolate concepts into the smallest units possible, and deeply understand each one individually. Learning concepts atomically is a powerful technique as it removes distractions and allows you to focus on each concept individually. This allows you to quickly form units of understanding that can be easily remembered, used to help understand closely related concepts, and more. You can quickly locate missing knowledge or what concept is giving you the actual trouble. That this method of isolation is precisely a powerful problem solving technique is no coincidence. Once you understand each concept individually, then relate those concepts.
        - Active Recall
            - The forgetting curve is an unfortunate necessity, perhaps due to the sheer amount of information our brains receive on a daily basis [3]. Active recall not only improves ability to store information in long-term memory, but also improves recollection of that concept. Active recall is the process of recalling, or consciously remembering some information. I find active recall one of the most reliable and consistent ways to ensure I will remember a concept. If you can't recall details immediately after learning them, what makes you think you can recall them the next day? Active recall will highlight what you know, and more importantly, what you don't.
            - Also, explain in your own words, this ensures you understand the actual meaning of the concept, and you are not simply memorizing the words.
        - Intellectual Integrity
            - Identify what you don't know, what knowledge you are missing?, any holes in your logic?, does the concept even make sense?, do you actually understand this?, do you know the entire argument or only the conclusion?. Be honest with yourself. Sometimes referred to as illusions of competence. [The illusion of knowing: Failure in the self-assessment of comprehension].
            - Any holes in your knowledge are pointing to what you should learn next.
        - Simplify
            - Perhaps the most important step. Starting with the concept with all its detail, progressively simplify it by removing technical words, uncessary words, etc. Ultimately extract only the crux of the concept, usually either a word, phrase, or sentence.
        - Repeat
            - Do this until you are satisfied with your understanding. It helps to occasionally repeat steps long after you learned the concept, incase you learn any thing new, and to fight the forgetting curve.
        - Also known as...
            - Active learning
            - Feynman Technique (Richard Feynmann)
            - Chunk (A Mind for Numbers: How to excel at math and science - Barbara Oakley)
            - First principles (Elon Musk)
    - Relating multiple concepts
        - Define a context
            - View a context as the location of a tree in a forest, concepts as branches on the tree, and details as leaves on the tree (note leaves tend to be on the very outside of a tree, but not always).
        - Build the tree
            - Within some context, extract a concept into its most fundamental pieces, it's essence. These pieces are concepts which form new branches, originating from the initial concept. Some of these new concepts form new and useful contexts, a seedling. Others may be a further abstraction of the initial concept within the context, forming a deeper meaning. This requires focus, you must be aware of the current context, concept, and related concepts. If you get stuck abstracting a concept, refocus on the concept and move to another location either on the tree or another context, there are more fish to fry.
            - Asking 'why?' delves deeper into the motivation of the concept, which is usually a more abstract and deeper concept.
        - Relate
            - Compare concepts that appear in multiple contexts.
        - Also known as...
            - Circle of competence (Warren Buffet)
                - A context which you are highly knowledgable in is your circle of competence.
            - Semantic Tree (Elon Musk)
                - Think 'Conceptual Tree'
            - Common Sense
                - Not common or easy
                - Requires deep intuition and creativity, not experience. Essentially critical thinking.
                - Example: Not touching a hot pan requires common sense. Sure, through experience you may learn not to touch a hot pan, but to know not to touch one before experiencing requires deeper knowledge of how the world works. You must understand the basics of heat transfer. That the pan gets hot because it is receiving heat from the stove, and touching the pot transfers the heat from the pan to you, burning you.
`}</ReactMarkdown>
    </div>
  </div>
)
