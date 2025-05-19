<script lang="ts">
	import { Howl, Howler } from 'howler';

    let tracks = {
        'lookatyou-before': {
            label: 'Final Mix',
            name: 'Look At You',
            file: '/audio/look-at-you-preview.wav'
        },
        'lookatyou-after': {
            label: 'Mastered',
            name: 'Look At You',
            file: '/audio/look-at-you-preview-master.wav',
        },
        'sleepwalker-before': {
            label: 'Unmixed',
            name: 'Sleepwalkers',
            file: '/audio/sleep-walkers-preview.wav'
        },
        'sleepwalker-after': {
            label: 'Mastered',
            name: 'Sleepwalkers',
            file: '/audio/sleep-walkers-preview-master.wav',
        }
    }

    for (const track of Object.values(tracks)) {
        track.sound = new Howl({
            src: track.file,
            volume: 0.25,
        })
    }

    let playingTrack = undefined
    function getTrackSound(track) {
        return tracks[track].sound
    }

	function playSound(track) {
        const sound = getTrackSound(track)
        if (sound.playing()) {
            playingTrack = undefined
            Howler.stop()
        } else {
            Howler.stop()
            playingTrack = track
            sound.play()
        }
	}
</script>

<section class="mastering-comparison">
    <div class="wrapper">
        <h2>Track Comparisons</h2>
        {#each [
            {before: 'lookatyou-before', after: 'lookatyou-after' },
            {before: 'sleepwalker-before', after: 'sleepwalker-after' },
            ] as {before, after}, i}
            {#if i !== 0}
                <div class="divider" />
            {/if}
            <div class="track-preview">
                <span class="track-name">
                    {tracks[before].name}
                </span>
                <button class="before" class:is-playing={playingTrack === before} on:click={() => playSound(before)}>{tracks[before].label}</button>
                <button class="after" class:is-playing={playingTrack === after} on:click={() => playSound(after)}>{tracks[after].label}</button>
            </div>
        {/each}
                </div>
</section>


<style lang="scss">
    .mastering-comparison {
        margin: 4rem auto;
        margin-bottom: 8rem;
        /* background: var(--c-gray); */
        max-width: var(--screen-md);
        width: 100%;
        display: flex;
        flex-direction: column;
        align-items: center;

        .wrapper {
            padding: 4rem 0;
            width: 100%;
            max-width: var(--screen-md);
            display: flex;
            flex-direction: column;
            align-items: center;
        }

        h2 {
            align-self: start;
            margin-bottom: 4rem;
        }

        .divider {
            width: 100%;
            height: 1px;
            background-color: white;
            margin: 2rem 0;
        }

        .track-preview {
            display: grid;
            width: 100%;
            grid-template:
                "name before after"  auto
                / 1fr auto auto;
            --gradient-rotation: 65deg;
            --before-color-end: #DA9B76;
            --after-color-end: #D68E7C;

            .track-name {
                grid-area: name;
                font-family: var(--font-secondary);
                align-self: center;
                font-size: 1.5rem;
            }

            .before {
                grid-area: before;
                background: linear-gradient(var(--gradient-rotation), var(--c-pri), var(--before-color-end));
                filter: saturate(0.35);
            }

            .after {
                grid-area: after;
                background: linear-gradient(var(--gradient-rotation), var(--after-color-end), #6b3bff);
                filter: saturate(0.35);
            }

            .is-playing {
                filter: none;
            }

            button {
                width: 12rem;
                height: 7.25rem;
                margin: 0.25rem;
                border: none;
                border-radius: 0.25rem;
                /* background: var(--c-gray); */
                padding: 1rem;
                color: white;
                font-size: 2rem;
                cursor: pointer;
            }
        }
    }

    @media (max-width: 1000px) {
        .mastering-comparison {
            padding: 0 2rem;
            h2 {
                font-size: 3rem;
            }

            .track-preview {
                display: flex;
                flex-direction: column;
                align-items: center;

                .before {
                    grid-area: before;
                    background: linear-gradient(170deg, var(--c-pri), var(--before-color-end));
                    filter: saturate(0.35);
                }

                .after {
                    grid-area: after;
                    background: linear-gradient(170deg, var(--after-color-end), #6b3bff);
                    filter: saturate(0.35);
                }

                .track-name {
                    font-size: 1.5rem;
                    padding: 1rem 0 2rem;
                }

                button {
                    width: 100%;
                    height: auto;
                    padding: 1rem 0.5rem;
                    font-size: 1.5rem;
                }
            }
        }
    }

</style>
