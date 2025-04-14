import { dirname, resolve } from 'node:path'
import { fileURLToPath} from 'node:url'
import { defineConfig } from 'vite'

const __dirname = dirname(fileURLToPath(import.meta.url))

// https://vitejs.dev/config/
export default defineConfig({
    clearScreen: false,
    build: {
        rollupOptions: {
            input: {
                main: resolve(__dirname, 'index.html'),
                floerpmun: resolve(__dirname, 'floerpmun.html'),
                mini_floerpmun: resolve(__dirname, 'mini_floerpmun.html')
            }
        }
    },
    server: {
        watch: {
            ignored: [
                "**/*.fs" // Don't watch F# files
            ]
        }
    }
})
