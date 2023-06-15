vim.keymap.set("n", "<leader>gs", vim.cmd.Git, { desc = "Git status" })

local NikolovLazar_Fugitive = vim.api.nvim_create_augroup("NikolovLazar_Fugitive", {})

local autocmd = vim.api.nvim_create_autocmd
autocmd("BufWinEnter", {
    group = NikolovLazar_Fugitive,
    pattern = "*",
    callback = function()
        if vim.bo.ft ~= "fugitive" then
            return
        end

        local bufnr = vim.api.nvim_get_current_buf()

        vim.keymap.set("n", "<leader>gc", function()
            vim.cmd("Gwrite")
        end, { buffer = bufnr, remap = false, desc = "Git commit" })

        vim.keymap.set("n", "<leader>gp", function()
            vim.cmd.Git('push')
        end, { buffer = bufnr, remap = false, desc = "Git push" })

        vim.keymap.set("n", "<leader>gP", function()
            vim.cmd.Git({ 'pull', '--rebase' })
        end, { buffer = bufnr, remap = false, desc = "Git pull --rebase" })

        vim.keymap.set("n", "<leader>gt", ":Git push -u origin",
            { buffer = bufnr, remap = false, desc = "Git push -u origin" })
    end
})
