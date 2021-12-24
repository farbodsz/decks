import { SaveRounded } from "@mui/icons-material";
import MenuIcon from "@mui/icons-material/Menu";
import AppBar from "@mui/material/AppBar";
import Box from "@mui/material/Box";
import Button from "@mui/material/Button";
import IconButton from "@mui/material/IconButton";
import Toolbar from "@mui/material/Toolbar";
import Typography from "@mui/material/Typography";
import React from "react";

interface AppBarProps {
  onSave: (event: React.MouseEvent<HTMLButtonElement>) => void;
}

export default function DecksAppBar(props: AppBarProps) {
  return (
    <Box sx={{ flexGrow: 1 }}>
      <AppBar position="static">
        <Toolbar>
          <IconButton
            size="large"
            edge="start"
            color="inherit"
            aria-label="menu"
            sx={{ mr: 2 }}
          >
            <MenuIcon />
          </IconButton>
          <Typography variant="h6" component="div" sx={{ flexGrow: 1 }}>
            Decks Editor
          </Typography>
          <Button
            color="inherit"
            endIcon={<SaveRounded />}
            onClick={(e) => props.onSave(e)}
          >
            Save
          </Button>
        </Toolbar>
      </AppBar>
    </Box>
  );
}
