<script>
	import Slide from "$lib/Slide.svelte";
	import Deck from "$lib/Deck.svelte";
    import Application from '$lib/Application.svelte';
    // import VideoPlayer from 'svelte-video-player';
    /* <VideoPlayer source={["https://www.youtube.com/embed/xkqflKC64IM?start=333&end=395" ]} /> */
</script>

<style>
</style>

<Deck>

    <Slide>
        <div class="flex flex-col flex-wrap justify-evenly mx-4 mt-4">
            <div class="w-full mb-8">
                <h1
                    class="font-sans font-black text-neutral-900 text-5xl text-left tracking-tighter"
                    >
                    Let's have some fun!
                </h1>
            </div>
            <div class="w-full">
                <div
                    class="flex flex-wrap justify-evenly items-top"
                    >
                    <ul class="text-2xl text-gray-700 list-disc">
                        {#each [
                            { label: "Writer", description: "Automatically generate stories, business pitches, etc.", links: [
                            "https://app.inferkit.com/demo",
                            "https://rytr.me/",
                            "https://www.plot-generator.org.uk/create.php?type=8",
                            "https://novelai.net/stories",
                            "https://sassbook.com/ai-story-writer",
                            ]},
                            { label: "Motivational Quote", description: "For days when you don't want to do anything", links: [
                            "https://inspirobot.me/",
                            "https://boredhumans.com/quotes.php"
                            ]},
                            { label: "Memer", description: "Haha memes go burr", links: [
                            "https://imgflip.com/ai-meme",
                            ]},
                            { label: "Style Transfer", description: "Merge the content of one image and the style of another", links: [
                            "https://style-transfer.makeoptim.com/",
                            "https://reiinakano.com/arbitrary-image-stylization-tfjs/",
                            "https://deepdreamgenerator.com/",
                            ]},
                            { label: "Deep Dream", description: "I can't easily explain this one", links: [
                            "https://deepdreamgenerator.com/",
                            ]},
                            { label: "Image Generation", description: "Create dope images", links: [
                            { href: "https://huggingface.co/spaces/dalle-mini/dalle-mini", label: "DALL-E mini" },
                            { href: "https://www.craiyon.com/", label: "DALL-E mini 2" },
                            { href: "http://gaugan.org/gaugan2/", label: "GauGAN2: Turn your doodles into reality"},
                            ]},
                            { label: "Misc", description: "", links: [
                            {href: "https://hotpot.ai/", label: "Make and personalize art, remove objects/backgrounds, restore/colorize images, etc."},
                            { href: "https://www.youtube.com/watch?v=WHwQeetjLwk", label: "Elon Musk's face on a baby (viewer discretion is advised)"},
                            { href: "https://fakeyou.com/", label: "Text to Speech using voices from tv-shows, movies, musicians, etc." }
                            ]},
                            ]
                            as category
                            }
                            <strong class="text-2xl text-gray-800 list-disc">
                                {category.label}
                            </strong>
                            <div class="-mt-1 mb-1 text-sm leading-6">
                            <span class="text-sm leading-6 -mt-1 text-gray-600">
                                {category.description}
                            </span>
                            </div>
                            <ul class="text-xl mb-8 text-gray-700">
                                {#each category.links as link}
                                    {#if link.href}
                                        <li><a target="_blank" href={link.href}>{link.label}</a></li>
                                    {:else}
                                        <li><a target="_blank" href={link}>{link}</a></li>
                                    {/if}
                                {/each}
                            </ul>
                        {/each}
                    </ul>
                </div>
            </div>
        </div>
    </Slide>


    <Slide>
        <div class="flex flex-col min-h-screen justify-center">
            <h1
                class="font-sans font-black text-neutral-900 text-center text-7xl tracking-tighter max-w-5/6 mx-auto my-8 leading-none"
            >
                Artificial Intelligence
            </h1>
            <h2 class="text-center">
                Trey Wilkinson
            </h2>
        </div>
    </Slide>

	<Slide>
		<div
			class="flex flex-wrap justify-evenly items-center mx-auto max-w-22/24 pt-20"
		>
			<div class="lg:w-11/24 w-22/24 animate__animated animate__fadeInUp">
				<h1
					class="font-sans font-black text-neutral-900 text-5xl text-left mt-8 tracking-tighter"
				>
                    What is AI?
				</h1>
                <ul class="font-sans font-light text-xl text-gray-800 mt-8 leading-relaxed list-disc list-inside">
                    <li>Aritificial intelligence := the theory and development of computer systems able to perform tasks that normally require human intelligence, such as visual perception, speech recognition, decision-making, and translation between languages.</li>
                    <li>Questions/Goals: What is intelligence, General AI, Ethics</li>
                    <li>Relatively new field (1950s)</li>
                    <li>Related domains</li>
                    <li>Key players: DeepMind, OpenAI, Tech companies, Numenta, NVIDIA</li>
                </ul>
			</div>

			<div
				class="lg:w-11/24 w-22/24 animate__animated animate__fadeInUp"
			>
				<img src="cognitive-science.svg" alt="figure-1" class="w-128 h-128" />
			</div>
		</div>
	</Slide>

	<Slide>
		<div class="flex flex-wrap justify-evenly mx-auto max-w-22/24 pt-20">
			<div class="xl:w-1/3 w-22/24">
				<h1
					class="font-sans font-black text-neutral-900 text-5xl text-left mt-8 tracking-tighter"
				>
                    Why AI?
				</h1>
				<p
					class="font-sans font-light text-xl text-gray-500 mt-8 leading-relaxed"
				>
                It's awesome and useful
				</p>
			</div>
			<div class="xl:w-2/3 w-22/24">
				<div
					class="flex flex-wrap justify-evenly items-top mx-auto max-w-22/24"
				>
                {#each [
                    {emojii: "thumbs-up", label: 'Benefits', content: ['Solve problems (easy to state, hard to formalize)', 'Enhance people']},
                    {emojii: "thumbs-down", label: 'Negatives', content: ['"With great power comes great responsibility"', 'Take jobs', 'Bad training data'] },
                    {label: 'Future', content: ['AI as a technology', 'By 2025, 95% of customer interactions will be powered by AI.', 'The percentage of enterprises employing AI grew 270% over the past four years.']},
                    {label: 'Intellectually interesting', content: ['What is cognition?', 'What is intelligence?']},
                    {label: 'Who uses AI?', content: ['Businesses', 'Creatives', 'Everyone']},
                    ] as card, i}
					<div
         class="w-23/24 md:w-11/24 py-10 animate__animated op0 start-{i + 1}:op100 start-{i + 1}:animate__fadeInRight"
					>
						<!-- <div
							class="i-openmoji-magnifying-glass-tilted-right
						text-5xl"
						/>
                        -->
						<h2
							class="font-sans font-semibold text-neutral-900 mt-6 text-2xl tracking-tighter leading-none"
						>
                        {card.label}
						</h2>
                        <ul
							class="font-sans font-light text-gray-500 mt-4 leading-relaxed"
                            >
                        {#each card.content as line}
                            <li>{line}</li>
                        {/each}
                        </ul>
					</div>
                {/each}
			</div>
		</div>
	</Slide>

	<Slide>
		<div class="flex flex-col min-h-screen pt-15 ml-8">
            <!--
			<p
				class="font-sans font-bold text-sm text-blue-600 tracking-widest uppercase"
			>
				Thanks to
			</p>
            -->
			<h1
				class="font-sans font-black text-neutral-900 text-left text-6xl tracking-tighter mt-4 mb-8 leading-none"
			>
				Awesome Applications
			</h1>
			<div
				class="flex flex-wrap max-h-full max-w-full justify-evenly mx-6"
			>
				<!-- bg-gray-100 -->
                {#each [
                        { label: 'Video games', content: [ 'Above human-level performance at: Atari games, StarCraft II, Chess, Go, Shogi, Dota 2, Minecraft', 'Why video games?']},
                        { label: 'Voice generation and recognition', content: ['Mimic human voice', 'Music synthesis', 'Lip reading']},
                        { label: 'Language', content: ['Chatbot', 'Content writing (blog posts, news article, social media post, etc.)']},
                        { label: 'Image creation/manipulation', content: ['Content creation', 'Image restoration', 'Image upscaling']},
                        { label: 'Deep fakes', content: ['Choose your actor', 'Bank heist?']},
                        { label: 'Healthcare', content: ['Detect, predict, and diagnose diseases', 'Develop cures to diseases (protein folding)']},
                        { label: 'Others', content: ['Self driving cars']},
                    ] as card, i}
				<div
        class="xl:w-11/36 md:w-8/18 border border-gray-200 p-6 m-4 ml-0 rounded-lg shadow-md start-{i+1}:animate__animated start-{i+1}:animate__pulse start-{i+1}:bg-gray-100"
				>
                <!--
					<div class="i-openmoji-shooting-star text-5xl" />
                -->
					<h2
						class="font-sans font-semibold text-neutral-900 mt-6 text-2xl tracking-tighter leading-none"
					>
                    {card.label}
					</h2>
					<ul
						class="font-sans font-light text-gray-700 mt-4 leading-relaxed"
					>
                    {#each card.content as line}
                        <li>{line}</li>
                    {/each}
					</ul>
				</div>
                {/each}
			</div>
		</div>
	</Slide>

    <Application title="Motivational Quotes" />
    {#each [
        { src: "/inspirational-quote-13.jpg", description: "bedroom"},
        { src: "/inspirational-quote-12.jpg", description: "opium"},
        { src: "/inspirational-quote-4.jpg", description: "happy or fine"},
        { src: "/inspirational-quote-10.jpg", description: "peasant"},
        { src: "/inspirational-quote-7.jpg", description: "embrace alternative cheese"},
        ] as image}
        <Application title="Motivational Quotes">
            <img alt={image.description} src={image.src} />
        </Application>
    {/each}


    <Application title="Memes" />
    {#each [
        { src: "/ai-generated-meme-14.png", description: "Your not a plant"},
        { src: "/ai-generated-meme-11.png", description: "planet. no people in forest"},
        { src: "/ai-generated-meme-16.png", description: "new meme. chicken nuggets"},
        { src: "/ai-generated-meme-17.png", description: "i dont always vote"},
        { src: "/ai-generated-meme-3.png", description: "stranger or friend"},
        { src: "/ai-generated-meme-18.png", description: "helicoptor"},
        ] as image}
        <Application title="Memes">
            <img alt={image.description} src={image.src} />
        </Application>
    {/each}

    <Application title="DALL-E 2" description="Creates an image based on a text description" />
    {#each [
        { description: "An astronaut playing basketball with cats in space as a children's book illustration", src: "/astronaut-cats-illustration.jpg" },
        { description: "Teddy bears mixing sparkling chemicals as mad scientists in a steampunk style", src: "/bears-chemicals-steampunk.jpg"},
        { description: "A bowl of soup as a planet in the universe as a 1960s poster", src: "/soup-planet-1960s.jpg"} ,
        { description: "A bowl of soup that is a portal to another dimension drawn on a cave wall", src: "/soup-portal-cave.jpg"},
        ] as image}
        <Application title="DALL-E 2" description={image.description}>
            <img alt={image.description} src={image.src} />
        </Application>
    {/each}

    <Application title="GauGAN2" description="Creates realistic images from doodles" />
    {#each [
        { description: "", src: "/gaugan2-1.jpeg" },
        { description: "", src: "/gaugan2-1.png"},
        { description: "", src: "/gaugan2-2.png"} ,
        { description: "", src: "gaugan2-3.jpg"},
        { description: "", src: "gaugan2-4.webp"},
        { description: "", src: "gaugan2-5.jpeg"},
        ] as image}
        <Application title="GauGAN2">
            <img alt={image.description} src={image.src} />
        </Application>
    {/each}

    <Application title="Deep Fakes" description="">
        <iframe width="1900" height="736" src="https://www.youtube.com/embed/xkqflKC64IM?start=334&end=395" title="Best Of Deep Fakes Compilation" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
    </Application>

    <Application title="Style transfer" description="Combine the artistic style of one image and the content of another" />
    {#each [
        { description: "", src: "/style-transfer-1.jpeg" },
        { description: "", src: "/style-transfer-2.png" },
        { description: "", src: "/style-transfer-3.png" },
        ] as image}
        <Application title="Style transfer">
            <img alt="" src={image.src} />
        </Application>
    {/each}

    <Application title="StyleGAN" description="Generate images indistinguishable from others" />
    {#each [
        { description: "", src: "/stylegan2-1.png" },
        { description: "", src: "/stylegan2-2.png" },
        ] as image}
        <Application title="StyleGAN">
            <img alt="" src={image.src} />
        </Application>
    {/each}

    <Application title="StyleGAN">
        <iframe width="1900" height="736" src="https://www.youtube.com/embed/9QuDh3W3lOY?start=70&end=120" title="Synthesizing High-Resolution Images with StyleGAN2" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
    </Application>

</Deck>
