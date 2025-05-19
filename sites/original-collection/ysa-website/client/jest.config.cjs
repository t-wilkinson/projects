module.exports = {
	transform: {
		'^.+\\.js$': 'babel-jest',
		'^.+\\.svelte$': ['svelte-jester', { preprocess: true }],
        '^.+\\.ts$': 'ts-jest',
		'.+\\.(css|styl|less|sass|scss|png|jpg|ttf|woff|woff2)$': 'jest-transform-stub'
	},
	moduleFileExtensions: ['svelte', 'ts', 'js'],
	testEnvironment: 'jsdom',
	setupFilesAfterEnv: ['@testing-library/jest-dom/extend-expect']
};
