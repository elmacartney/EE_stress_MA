;(function () {
    try {
        const stored_user = JSON.parse(window.localStorage && window.localStorage.getItem('User'))

        const THEME_MAPPING = {
            1: 'noir',
            2: 'neutral',
            3: 'tangerine',
            4: 'sunflower',
            5: 'clover',
            6: 'blueberry',
            7: 'sky',
            8: 'amethyst',
            9: 'graphite',
            10: 'gold',
            11: 'dark',
            12: 'pink',
            13: 'royal_blue',
        }

        let theme = null

        // Try to see if a theme is forced
        const params = new URLSearchParams(window.location.search)
        const paramsTheme = params.get('theme')
        if (paramsTheme) {
            try {
                theme = parseInt(paramsTheme)
            } catch (e) {}
        }

        if (theme === null && stored_user) {
            theme = stored_user.theme
        }

        if (theme) {
            // Always set the theme first from local storage, to avoid theme flashes.
            // Later on the most updated theme value from the sync request
            // can be used to update the app.
            const theme_cls = 'theme_' + THEME_MAPPING[theme]
            const html_element = document.documentElement

            html_element.className = html_element.className + ' ' + theme_cls
        }
    } catch (error) {
        // this file is not transpiled, so in older browsers some
        // errors can be thrown.
    }
})()
